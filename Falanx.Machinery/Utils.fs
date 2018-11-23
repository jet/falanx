namespace Falanx.Machinery
    module Utils =
        open System
        open System.Collections.Generic
        open System.Text.RegularExpressions
        open System.Reflection
        open System.Runtime.Serialization
        open Microsoft.FSharp.Quotations
        open Microsoft.FSharp.Quotations.Patterns
        open Microsoft.FSharp.Reflection
        open Microsoft.FSharp.Compiler.Ast
        open Microsoft.FSharp.Compiler.Range
        open Microsoft.FSharp.Compiler
        open Microsoft.FSharp.Compiler.SourceCodeServices
        open ProviderImplementation.ProvidedTypes
        open ProviderImplementation.ProvidedTypes.UncheckedQuotations
        open Falanx.Machinery.Reflection
        
        let thisPrefix = "x"
        
        let inline notImpl<'T> e : 'T = raise <| NotImplementedException(sprintf "%O" e)
        
        let hset (ts : seq<'T>) = System.Collections.Generic.HashSet<_>(ts)
        
        let isListType (t : Type) =
            t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>>
        
        let isOptionType (t : Type) =
            t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>
        
        let wrapDelegate<'Dele when 'Dele :> Delegate> (m : MethodInfo) =
            Delegate.CreateDelegate(typeof<'Dele>, m) :?> 'Dele
        
        /// taken from mscorlib's Tuple.GetHashCode() implementation
        let inline private combineHash (h1 : int) (h2 : int) =
            ((h1 <<< 5) + h1) ^^^ h2
        
        /// pair hashcode generation without tuple allocation
        let inline hash2 (t : 'T) (s : 'S) =
            combineHash (hash t) (hash s)
            
        /// triple hashcode generation without tuple allocation
        let inline hash3 (t : 'T) (s : 'S) (u : 'U) =
            combineHash (combineHash (hash t) (hash s)) (hash u)
        
        /// quadruple hashcode generation without tuple allocation
        let inline hash4 (t : 'T) (s : 'S) (u : 'U) (v : 'V) =
            combineHash (combineHash (combineHash (hash t) (hash s)) (hash u)) (hash v)
        
        type MemberInfo with
            member m.TryGetCustomAttribute<'Attr when 'Attr :> System.Attribute> () =
                try
                    let attrs = m.GetCustomAttributes(typeof<'Attr>, false)
                    match attrs with
                    | null -> None
                    | _ -> if Seq.isEmpty attrs then None
                           else  
                                attrs
                                |> Seq.choose (fun x -> match x with :? 'Attr as n -> Some n | _ -> None)
                                |> Seq.tryHead
                                //Some((Seq.head attrs) :?> 'Attr)
                with _ ->
                    printfn "TryGetCustomAttribute failed for attribute %A on type %A" typeof<'Attr>.Name m
                    None
        
            member m.ContainsAttribute<'Attr when 'Attr :> System.Attribute> () =
                m.GetCustomAttributes<'Attr> () |> Seq.isEmpty |> not
        
            member m.Assembly = match m with :? Type as t -> t.Assembly | _ -> m.DeclaringType.Assembly
        
            member m.GetCompilationRepresentationFlags() =
                match m.TryGetCustomAttribute<CompilationRepresentationAttribute>() with
                | None -> CompilationRepresentationFlags.None
                | Some attr -> attr.Flags
        
        type MethodBase with
            member m.GetOptionalParameterInfo () =
                let parameters = m.GetParameters()
                parameters
                |> Seq.map (fun p ->
                    try
                        if (p.CustomAttributes |> Seq.isEmpty) || p.GetCustomAttributes<OptionalArgumentAttribute>() |> Seq.isEmpty then None
                        else
                            Some p.Name
                    with _ -> None)
                |> Seq.toList
        
        /// build a range value parsing the Expr.CustomAttributes property.
        let tryParseRange (expr : Expr) =
            match expr.CustomAttributes with
            | [ NewTuple [_; NewTuple [ Value (:? string as file, _)
                                        Value (:? int as r1, _)
                                        Value (:? int as c1, _)
                                        Value (:? int as r2, _)
                                        Value (:? int as c2, _) ]]] -> 
                let p1 = mkPos r1 c1
                let p2 = mkPos r2 c2
                Some <| mkRange file p1 p2
            | _ -> None
        
        let inline mkIdent range text = new Ident(text, range)
        let inline mkLongIdent range (li : Ident list) = LongIdentWithDots(li, [range])
        let inline mkVarPat range (v : Quotations.Var) = 
            let lident = mkLongIdent range [mkIdent range v.Name]
            SynPat.LongIdent(lident, None, None, SynConstructorArgs.Pats [], None, range)
        
        let inline mkBinding range isTopLevelValue pat expr =
            let synValData = SynValData.SynValData(None, SynValInfo((if isTopLevelValue then [] else [[]]), SynArgInfo([], false, None)), None)
            SynBinding.Binding(None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty, synValData, pat, None, expr, range0, SequencePointAtBinding range)
        
        let mkUniqueIdentifier range =
            let suffix = Guid.NewGuid().ToString("N")
            let name = "_bind_" + suffix
            mkIdent range name
        
        let private moduleSuffixRegex = new Regex(@"^(.*)Module$", RegexOptions.Compiled)
        let private fsharpPrefixRegex = new Regex(@"^FSharp(.*)(`[0-9]+)?$", RegexOptions.Compiled)
        /// recover the F# source name for given member declaration
        let getFSharpName (m : MemberInfo) =
            match m.TryGetCustomAttribute<CompilationSourceNameAttribute> () with
            | Some a ->
                a.SourceName
            | None ->
        
            // this is a hack; need a better solution in the long term
            // see https://visualfsharp.codeplex.com/workitem/177
            if m.Assembly = typeof<int option>.Assembly && fsharpPrefixRegex.IsMatch m.Name then
                let rm = fsharpPrefixRegex.Match m.Name
                let group = rm.Groups.[1].Value
                let demangled = Microsoft.FSharp.Compiler.PrettyNaming.DemangleGenericTypeName group
                demangled
            elif m.Name = "DefaultAsyncBuilder" && m.Assembly = typeof<int option>.Assembly then
                "async"
            else
        
            match m, m.TryGetCustomAttribute<CompilationRepresentationAttribute> () with
            | :? Type as t, Some attr when attr.Flags.HasFlag CompilationRepresentationFlags.ModuleSuffix && FSharpType.IsModule t ->
                let rm = moduleSuffixRegex.Match m.Name
                if rm.Success then rm.Groups.[1].Value
                else
                    m.Name
            | _ -> m.Name.Split('`').[0]
        
        /// generate full path for given memberinfo
        let getMemberPath range (m : MemberInfo) (knownNamespaces : _ Set) (ommitEnclosingType : Type option) =
            let rec aux (m : MemberInfo) = seq {
                match m.DeclaringType with
                | null -> 
                    match (m :?> Type).Namespace with
                    | null -> ()
                    | ns ->
                        if knownNamespaces.Contains ns then ()
                        else yield! ns.Split('.') 
                | dt -> 
                    match ommitEnclosingType with
                    | Some(et) when et.Name = dt.Name -> ()
                    | _ -> yield! aux dt 
            
                yield getFSharpName m
            }
        
            aux m |> Seq.map (mkIdent range) |> Seq.toList
        
        /// converts a System.Type to a F# compiler SynType expression
        let rec sysTypeToSynType (range : range) (t : System.Type) knownNamespaces (ommitEnclosingType : Type Option) : SynType =
            if FSharpType.IsTuple t then
                let telems = 
                    FSharpType.GetTupleElements t 
                    |> Array.toList
                    |> List.map(fun et -> false, sysTypeToSynType range et knownNamespaces ommitEnclosingType)
        
                SynType.Tuple(telems, range)
            elif t.GetType().Name <> typeof<ProvidedUnion>.Name && FSharpTypeSafe.IsFunction t then
                let dom, cod = FSharpTypeSafe.GetFunctionElements t
                let synDom = sysTypeToSynType range dom knownNamespaces ommitEnclosingType
                let synCod = sysTypeToSynType range cod knownNamespaces ommitEnclosingType
                SynType.Fun(synDom, synCod, range)
            elif t.IsGenericType && not t.IsGenericTypeDefinition then
                let synDef = sysTypeToSynType range (t.GetGenericTypeDefinition()) knownNamespaces ommitEnclosingType
                let synParams = t.GetGenericArguments() |> Seq.map (fun arg -> sysTypeToSynType range arg knownNamespaces ommitEnclosingType ) |> Seq.toList
                SynType.App(synDef, None, synParams, [], None, (* isPostFix *) false, range)
            elif t.IsArray then
                let synElem = sysTypeToSynType range (t.GetElementType()) knownNamespaces ommitEnclosingType
                let rk = t.GetArrayRank()
                SynType.Array(rk, synElem, range)
            else
                let liwd = LongIdentWithDots(getMemberPath range t knownNamespaces ommitEnclosingType, [range])
                SynType.LongIdent liwd
        
        /// creates a union case identifier
        let mkUciIdent range (ucDeclaringType : Type) (ucName : string) knownNamespaces ommitEnclosingType=
            let ident =
                if ucDeclaringType.IsGenericType && ucDeclaringType.GetGenericTypeDefinition().Name = typeof<int option>.GetGenericTypeDefinition().Name then
                    [mkIdent range ucName]
                else
                    let path = 
                        let fullPath = getMemberPath range ucDeclaringType knownNamespaces ommitEnclosingType
                        List.truncate (fullPath.Length - 1) fullPath //do not include union type
                    path @ [mkIdent range ucName]
            
            LongIdentWithDots(ident, [range])
        
        /// recover curried function argument groupings for given method declaration
        let tryGetCurriedFunctionGroupings (m : MethodInfo) =
            if m.GetType().Name = "MethodSymbol2" then
                let found = 
                    m.GetCustomAttributesData()
                    |> Seq.tryFind (fun cad -> cad.AttributeType.Name = typeof<CompilationArgumentCountsAttribute>.Name )
                match found with
                | Some data ->
                    data.ConstructorArguments
                    |> Seq.collect (fun ca -> ca.Value
                                              :?> CustomAttributeTypedArgument seq
                                              |> Seq.map (fun cata -> cata.Value :?> int))
                    |> Seq.toList
                    |> Some
                | _ -> None
            else
                match m.TryGetCustomAttribute<CompilationArgumentCountsAttribute> () with
                | None -> None
                | Some a -> Some(a.Counts |> Seq.toList)
        
        /// converts a System.Reflection.MemberInfo to an SynExpr identifier
        let sysMemberToSynMember range (m : MemberInfo) knownNamespaces ommitEnclosingType =
            let liwd = 
                if PrettyNaming.IsMangledOpName m.Name then
                    let opName = getMemberPath range m knownNamespaces ommitEnclosingType |> List.last
                    LongIdentWithDots( [opName], [range])
                else
                    LongIdentWithDots(getMemberPath range m knownNamespaces ommitEnclosingType, [range])         
            SynExpr.LongIdent(false, liwd, None, range)
        
        /// creates a syntactic argument passed to method calls
        let mkArgumentBinding range optName synParam =
            match optName with
            | None -> synParam
            | Some name -> 
                let equality = SynExpr.Ident(mkIdent range "op_Equality")
                let ident = SynExpr.LongIdent(true, mkLongIdent range [mkIdent range name], None, range)
                let innerApp = SynExpr.App(ExprAtomicFlag.NonAtomic, true, equality, ident, range)
                SynExpr.App(ExprAtomicFlag.NonAtomic, false, innerApp, synParam, range)
        
        let isUnion (instance:Expr) =
            if instance.Type.GetType().Name = typeof<ProvidedUnion>.Name then true else
            FSharpType.IsUnion instance.Type
        
        
        /// recognizes bindings to union case fields in pattern matches
        /// Quotations directly access propertyInfo instances, which are
        /// not public. Recovers union metadata required for a proper pattern match expression
        /// in the F# ast.
        let (|UnionCasePropertyGet|_|) (expr : Expr) =
            match expr with
            | PropertyGet(Some instance, propertyInfo, []) when isUnion instance ->
                if isOptionType instance.Type then None
                elif isListType instance.Type then None
                // a property is a union case field \iff its declaring type is a proper subtype of the union type
                elif instance.Type = propertyInfo.DeclaringType then None
                else 
                    match propertyInfo.DeclaringType with 
                    | :? ProvidedTypeDefinition as puc -> 
                        match puc.GetProperties() |> Seq.tryFind (fun p -> p.Name = propertyInfo.Name) with 
                        | Some (pinfo) ->
                            match pinfo.CustomAttributes |> Seq.tryFind (fun x -> x.AttributeType = typeof<CompilationMappingAttribute>) with 
                            | Some attr -> 
                                Some(instance, pinfo.DeclaringType, pinfo.Name,attr.ConstructorArguments.[2].Value :?> int)
                            | None -> None 
                        | None -> None 
                    | _ -> 
                        // create a dummy instance for declaring type to recover union case info
                        let dummy = FormatterServices.GetUninitializedObject propertyInfo.DeclaringType
                        let uci,_ = FSharpValue.GetUnionFields(dummy, propertyInfo.DeclaringType, true)
                        match uci.GetFields() |> Array.tryFindIndex (fun p -> p = propertyInfo) with
                        | Some i -> Some(instance, uci.DeclaringType, uci.Name, i)
                        | None -> None
            | _ -> None
                                
        let mkUnionCaseInfo (typ: Type) (tag: int) (nameMapping: int -> string) =
            let ucType = typeof<Reflection.UnionCaseInfo>
            let ctor = ucType.GetConstructor(BindingFlags.Instance ||| BindingFlags.NonPublic, null, [| typeof<Type>; typeof<int> |], null)
            let unionCase = ctor.Invoke [| box typ; box tag |] :?> Reflection.UnionCaseInfo
            let namesField = ucType.GetField("names", BindingFlags.Instance ||| BindingFlags.NonPublic)
            namesField.SetValue(unionCase, Some(nameMapping))
            unionCase