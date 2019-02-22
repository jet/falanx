namespace Falanx.Machinery
module QuotationSimplifier =
    open System
    open System.Collections.Generic
    open System.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open ProviderImplementation
    open ProviderImplementation.ProvidedTypes
    open ProviderImplementation.ProvidedTypes.UncheckedQuotations

//--------------------------------------------------------------------------------
    // The quotation simplifier
    //
    // This is invoked for each quotation specified by the type provider, as part of the translation to 
    /// the target model, i.e. before it is handed to the F# compiler (for erasing type providers) or 
    // the TPSDK IL code generator (for generative type providers). This allows a broader range of quotations 
    // to be used when authoring type providers than are strictly allowed by those tools. 
    //
    // Specifically we accept:
    //
    //     - NewTuple nodes (for generative type providers)
    //     - TupleGet nodes (for generative type providers)
    //     - array and list values as constants
    //     - PropertyGet and PropertySet nodes
    //     - Application, NewUnionCase, NewRecord, UnionCaseTest nodes
    //     - Let nodes (defining "byref" values)
    //     - LetRecursive nodes
    //
    // Additionally, a set of code optimizations are applied for generative type providers:
    //    - inlineRightPipe
    //    - optimizeCurriedApplications
    //    - inlineValueBindings

    // Note, the QuotationSimplifier works over source quotations, not target quotations
    type QuotationSimplifier(isGenerated: bool) =

        let rec simplifyExpr q =
            match q with

#if !NO_GENERATIVE
            // Convert NewTuple to the call to the constructor of the Tuple type (only for generated types, 
            // the F# compile does the job for erased types when it receives the quotation)
            | NewTuple(items) when isGenerated ->
                let rec mkCtor args ty =
                    let ctor, restTyOpt = Reflection.FSharpValue.PreComputeTupleConstructorInfo ty
                    match restTyOpt with
                    | None -> Expr.NewObjectUnchecked(ctor, List.map simplifyExpr args)
                    | Some restTy ->
                        let curr = [for a in Seq.take 7 args -> simplifyExpr a]
                        let rest = List.ofSeq (Seq.skip 7 args)
                        Expr.NewObjectUnchecked(ctor, curr @ [mkCtor rest restTy])
                let tys = [ for e in items -> e.Type ]
                let tupleTy = ProvidedTypeBuilder.MakeTupleType(tys)
                simplifyExpr (mkCtor items tupleTy)

            // convert TupleGet to the chain of PropertyGet calls (only for generated types)
            | TupleGet(e, i) when isGenerated ->
                let rec mkGet ty i (e: Expr)  =
                    let pi, restOpt = Reflection.FSharpValueSafe.PreComputeTuplePropertyInfo(ty, i)
                    let propGet = Expr.PropertyGetUnchecked(e, pi)
                    match restOpt with
                    | None -> propGet
                    | Some (restTy, restI) -> mkGet restTy restI propGet
                simplifyExpr (mkGet e.Type i (simplifyExpr e))
#endif

            | Value(value, ty) ->
                if value |> isNull |> not then
                    let tyOfValue = value.GetType()
                    transValue(value, tyOfValue, ty)
                else q

            // Eliminate F# property gets to method calls
            | PropertyGet(obj,propInfo,args) ->
                match obj with
                | None -> simplifyExpr (Expr.CallUnchecked(propInfo.GetGetMethod(),args))
                | Some o -> simplifyExpr (Expr.CallUnchecked(simplifyExpr o,propInfo.GetGetMethod(),args))

            // Eliminate F# property sets to method calls
            | PropertySet(obj,propInfo,args,v) ->
                    match obj with
                    | None -> simplifyExpr (Expr.CallUnchecked(propInfo.GetSetMethod(),args@[v]))
                    | Some o -> simplifyExpr (Expr.CallUnchecked(simplifyExpr o,propInfo.GetSetMethod(),args@[v]))

            // Eliminate F# function applications to FSharpFunc<_,_>.Invoke calls
            | Application(f,e) ->
                simplifyExpr (Expr.CallUnchecked(simplifyExpr f, f.Type.GetMethod "Invoke", [ e ]) )

            // Eliminate F# union operations
            | NewUnionCase(ci, es) ->
                simplifyExpr (Expr.CallUnchecked(Reflection.FSharpValue.PreComputeUnionConstructorInfo ci, es) )

            // Eliminate F# union operations
            | UnionCaseTest(e,uc) ->
                let tagInfo = Reflection.FSharpValue.PreComputeUnionTagMemberInfo uc.DeclaringType
                let tagExpr =
                    match tagInfo with
                    | :? PropertyInfo as tagProp ->
                            simplifyExpr (Expr.PropertyGet(e,tagProp) )
                    | :? MethodInfo as tagMeth ->
                            if tagMeth.IsStatic then simplifyExpr (Expr.Call(tagMeth, [e]))
                            else simplifyExpr (Expr.Call(e,tagMeth,[]))
                    | _ -> failwith "unreachable: unexpected result from PreComputeUnionTagMemberInfo. Please report this bug to https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues"
                let tagNumber = uc.Tag
                simplifyExpr <@@ (%%(tagExpr): int) = tagNumber @@>

            // Eliminate F# record operations
            | NewRecord(ci, es) ->
                simplifyExpr (Expr.NewObjectUnchecked(Reflection.FSharpValue.PreComputeRecordConstructorInfo ci, es) )

            // Explicitly handle weird byref variables in lets (used to populate out parameters), since the generic handlers can't deal with byrefs.
            //
            // The binding must have leaves that are themselves variables (due to the limited support for byrefs in expressions)
            // therefore, we can perform inlining to translate this to a form that can be compiled
            | Let(v,vexpr,bexpr) when v.Type.IsByRef -> transLetOfByref v vexpr bexpr

            // Eliminate recursive let bindings (which are unsupported by the type provider API) to regular let bindings
            | LetRecursive(bindings, expr) -> simplifyLetRec bindings expr

            // Handle the generic cases
            | ShapeLambdaUnchecked(v,body) -> Expr.Lambda(v, simplifyExpr body)
            | ShapeCombinationUnchecked(comb,args) -> RebuildShapeCombinationUnchecked(comb,List.map simplifyExpr args)
            | ShapeVarUnchecked _ -> q

        and simplifyLetRec bindings expr =
            // This uses a "lets and sets" approach, converting something like
            //    let rec even = function
            //    | 0 -> true
            //    | n -> odd (n-1)
            //    and odd = function
            //    | 0 -> false
            //    | n -> even (n-1)
            //    X
            // to something like
            //    let even = ref Unchecked.defaultof<_>
            //    let odd = ref Unchecked.defaultof<_>
            //    even := function
            //            | 0 -> true
            //            | n -> !odd (n-1)
            //    odd  := function
            //            | 0 -> false
            //            | n -> !even (n-1)
            //    X'
            // where X' is X but with occurrences of even/odd substituted by !even and !odd (since now even and odd are references)
            // Translation relies on typedefof<_ ref> - does this affect ability to target different runtime and design time environments?
            let vars = List.map fst bindings
            let refVars = vars |> List.map (fun v -> Var(v.Name, ProvidedTypeBuilder.MakeGenericType(typedefof<_ ref>, [v.Type])))

            // "init t" generates the equivalent of <@ ref Unchecked.defaultof<t> @>
            let init (t:Type) =
                let r = match <@ ref 1 @> with Call(None, r, [_]) -> r | _ -> failwith "Extracting MethodInfo from <@ 1 @> failed"
                let d = match <@ Unchecked.defaultof<_> @> with Call(None, d, []) -> d | _ -> failwith "Extracting MethodInfo from <@ Unchecked.defaultof<_> @> failed"
                let ir = ProvidedTypeBuilder.MakeGenericMethod(r.GetGenericMethodDefinition(), [ t ])
                let id = ProvidedTypeBuilder.MakeGenericMethod(d.GetGenericMethodDefinition(), [ t ])
                Expr.CallUnchecked(ir, [Expr.CallUnchecked(id, [])])

            // deref v generates the equivalent of <@ !v @>
            // (so v's type must be ref<something>)
            let deref (v:Var) =
                let m = match <@ !(ref 1) @> with Call(None, m, [_]) -> m | _ -> failwith "Extracting MethodInfo from <@ !(ref 1) @> failed"
                let tyArgs = v.Type.GetGenericArguments()
                let im = ProvidedTypeBuilder.MakeGenericMethod(m.GetGenericMethodDefinition(), Array.toList tyArgs)
                Expr.CallUnchecked(im, [Expr.Var v])

            // substitution mapping a variable v to the expression <@ !v' @> using the corresponding new variable v' of ref type
            let subst =
                let map = [ for v in refVars -> v.Name, deref v ] |> Map.ofList
                fun (v:Var) -> Map.tryFind v.Name map

            let refExpr = expr.Substitute(subst)

            // maps variables to new variables
            let varDict = [ for (v, rv) in List.zip vars refVars -> v.Name, rv ] |> dict

            // given an old variable v and an expression e, returns a quotation like <@ v' := e @> using the corresponding new variable v' of ref type
            let setRef (v:Var) e =
                let m = match <@ (ref 1) := 2 @> with Call(None, m, [_;_]) -> m | _ -> failwith "Extracting MethodInfo from <@ (ref 1) := 2 @> failed"
                let im = ProvidedTypeBuilder.MakeGenericMethod(m.GetGenericMethodDefinition(), [ v.Type ])
                Expr.CallUnchecked(im, [Expr.Var varDict.[v.Name]; e])

            // Something like
            //  <@
            //      v1 := e1'
            //      v2 := e2'
            //      ...
            //      refExpr
            //  @>
            // Note that we must substitute our new variable dereferences into the bound expressions
            let body =
                bindings
                |> List.fold (fun b (v,e) -> Expr.Sequential(setRef v (e.Substitute subst), b)) refExpr

            // Something like
            //   let v1 = ref Unchecked.defaultof<t1>
            //   let v2 = ref Unchecked.defaultof<t2>
            //   ...
            //   body
            (body, vars)
            ||> List.fold (fun b v -> Expr.LetUnchecked(varDict.[v.Name], init v.Type, b)) 
            |> simplifyExpr


        and transLetOfByref v vexpr bexpr =
            match vexpr with
            | Sequential(e',vexpr') ->
                (* let v = (e'; vexpr') in bexpr => e'; let v = vexpr' in bexpr *)
                Expr.Sequential(e', transLetOfByref v vexpr' bexpr)
                |> simplifyExpr
            | IfThenElse(c,b1,b2) ->
                (* let v = if c then b1 else b2 in bexpr => if c then let v = b1 in bexpr else let v = b2 in bexpr *)
                //
                // Note, this duplicates "bexpr"
                Expr.IfThenElseUnchecked(c, transLetOfByref v b1 bexpr, transLetOfByref v b2 bexpr)
                |> simplifyExpr
            | Var _ ->
                (* let v = v1 in bexpr => bexpr[v/v1] *)
                bexpr.Substitute(fun v' -> if v = v' then Some vexpr else None)
                |> simplifyExpr
            | _ ->
                failwithf "Unexpected byref binding: %A = %A. Please report this bug to https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues" v vexpr

        and transValueArray (o: Array, ty: Type) =
            let elemTy = ty.GetElementType()
            let converter = getValueConverterForType elemTy
            let elements = [ for el in o -> converter el ]
            Expr.NewArrayUnchecked(elemTy, elements)

        and transValueList(o, ty: Type, nil, cons) =
            let converter = getValueConverterForType (ty.GetGenericArguments().[0])
            o
            |> Seq.cast
            |> List.ofSeq
            |> fun l -> List.foldBack(fun o s -> Expr.NewUnionCase(cons, [ converter(o); s ])) l (Expr.NewUnionCase(nil, []))
            |> simplifyExpr

        and getValueConverterForType (ty: Type) =
            if ty.IsArray then
                fun (v: obj) -> transValueArray(v :?> Array, ty)
            elif ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<_ list> then
                let nil, cons =
                    let cases = Reflection.FSharpType.GetUnionCases(ty)
                    let a = cases.[0]
                    let b = cases.[1]
                    if a.Name = "Empty" then a,b
                    else b,a

                fun v -> transValueList (v :?> System.Collections.IEnumerable, ty, nil, cons)
            else
                fun v -> Expr.Value(v, ty)

        and transValue (v: obj, tyOfValue: Type, expectedTy: Type) =
            let converter = getValueConverterForType tyOfValue
            let r = converter v
            if tyOfValue <> expectedTy then Expr.Coerce(r, expectedTy)
            else r

#if !NO_GENERATIVE
        let getFastFuncType (args: list<Expr>) resultType =
            let types =
                [|  for arg in args -> arg.Type
                    yield resultType |]
            let fastFuncTy =
                match List.length args with
                | 2 -> typedefof<OptimizedClosures.FSharpFunc<_, _, _>>.MakeGenericType(types) 
                | 3 -> typedefof<OptimizedClosures.FSharpFunc<_, _, _, _>>.MakeGenericType(types) 
                | 4 -> typedefof<OptimizedClosures.FSharpFunc<_, _, _, _, _>>.MakeGenericType(types) 
                | 5 -> typedefof<OptimizedClosures.FSharpFunc<_, _, _, _, _, _>>.MakeGenericType(types) 
                | _ -> invalidArg "args" "incorrect number of arguments"
            fastFuncTy.GetMethod("Adapt")

        let (===) a b = LanguagePrimitives.PhysicalEquality a b

        let traverse f =
            let rec fallback e =
                match e with
                | Let(v, value, body) ->
                    let fixedValue = f fallback value
                    let fixedBody = f fallback body
                    if fixedValue === value && fixedBody === body then
                        e
                    else
                        Expr.LetUnchecked(v, fixedValue, fixedBody)
                | ShapeVarUnchecked _ -> e
                | ShapeLambdaUnchecked(v, body) ->
                    let fixedBody = f fallback body
                    if fixedBody === body then
                        e
                    else
                        Expr.Lambda(v, fixedBody)
                | ShapeCombinationUnchecked(shape, exprs) ->
                    let exprs1 = List.map (f fallback) exprs
                    if List.forall2 (===) exprs exprs1 then
                        e
                    else
                        RebuildShapeCombinationUnchecked(shape, exprs1)
            fun e -> f fallback e

        let rightPipe = <@@ (|>) @@>
        let inlineRightPipe expr =
            let rec loop expr = traverse loopCore expr
            and loopCore fallback orig =
                match orig with
                | SpecificCall rightPipe (None, _, [operand; applicable]) ->
                    let fixedOperand = loop operand
                    match loop applicable with
                    | Lambda(arg, body) ->
                        let v = Var("__temp", operand.Type)
                        let ev = Expr.Var v

                        let fixedBody = loop body
                        Expr.Let(v, fixedOperand, fixedBody.Substitute(fun v1 -> if v1 = arg then Some ev else None))
                    | fixedApplicable -> Expr.Application(fixedApplicable, fixedOperand)
                | x -> fallback x
            loop expr


        let inlineValueBindings e =
            let map = Dictionary(HashIdentity.Reference)
            let rec loop expr = traverse loopCore expr
            and loopCore fallback orig =
                match orig with
                | Let(id, (Value(_) as v), body) when not id.IsMutable ->
                    map.[id] <- v
                    let fixedBody = loop body
                    map.Remove(id) |> ignore
                    fixedBody
                | ShapeVarUnchecked v ->
                    match map.TryGetValue v with
                    | true, e -> e
                    | _ -> orig
                | x -> fallback x
            loop e


        let optimizeCurriedApplications expr =
            let rec loop expr = traverse loopCore expr
            and loopCore fallback orig =
                match orig with
                | Application(e, arg) ->
                    let e1 = tryPeelApplications e [loop arg]
                    if e1 === e then
                        orig
                    else
                        e1
                | x -> fallback x
            and tryPeelApplications orig args =
                let n = List.length args
                match orig with
                | Application(e, arg) ->
                    let e1 = tryPeelApplications e ((loop arg)::args)
                    if e1 === e then
                        orig
                    else
                        e1
                | Let(id, applicable, (Lambda(_) as body)) when n > 0 ->
                    let numberOfApplication = countPeelableApplications body id 0
                    if numberOfApplication = 0 then orig
                    elif n = 1 then Expr.Application(applicable, List.head args)
                    elif n <= 5 then
                        let resultType =
                            applicable.Type
                            |> Seq.unfold (fun t ->
                                if not t.IsGenericType then None else
                                let args = t.GetGenericArguments()
                                if args.Length <> 2 then None else
                                Some (args.[1], args.[1])
                            )
                            |> Seq.toArray
                            |> (fun arr -> arr.[n - 1])

                        let adaptMethod = getFastFuncType args resultType
                        let adapted = Expr.Call(adaptMethod, [loop applicable])
                        let invoke = adapted.Type.GetMethod("Invoke", [| for arg in args -> arg.Type |])
                        Expr.Call(adapted, invoke, args)
                    else
                        (applicable, args) ||> List.fold (fun e a -> Expr.Application(e, a))
                | _ ->
                    orig
            and countPeelableApplications expr v n =
                match expr with
                // v - applicable entity obtained on the prev step
                // \arg -> let v1 = (f arg) in rest ==> f
                | Lambda(arg, Let(v1, Application(Var f, Var arg1), rest)) when v = f && arg = arg1 -> countPeelableApplications rest v1 (n + 1)
                // \arg -> (f arg) ==> f
                | Lambda(arg, Application(Var f, Var arg1)) when v = f && arg = arg1 -> n
                | _ -> n
            loop expr
#endif

        member __.TranslateExpression q = simplifyExpr q

        member __.TranslateQuotationToCode qexprf (paramNames: string[]) (argExprs: Expr[]) =
            // Use the real variable names instead of indices, to improve output of Debug.fs
            // Add let bindings for arguments to ensure that arguments will be evaluated
            let varDecisions = argExprs |> Array.mapi (fun i e -> match e with Var v when v.Name = paramNames.[i] -> false, v | _ -> true, Var(paramNames.[i], e.Type))
            let vars = varDecisions |> Array.map snd
            let expr = qexprf ([for v in vars -> Expr.Var v])

            let pairs = Array.zip argExprs varDecisions
            let expr = Array.foldBack (fun (arg, (replace, var)) e -> if replace then Expr.LetUnchecked(var, arg, e) else e) pairs expr
#if !NO_GENERATIVE
            let expr =
                if isGenerated then
                    let e1 = inlineRightPipe expr
                    let e2 = optimizeCurriedApplications e1
                    let e3 = inlineValueBindings e2
                    e3
                else
                    expr
#endif

            simplifyExpr expr
