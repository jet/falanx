namespace Falanx.Machinery
    module Expr =
        open Prelude
        open System
        open System.Collections
        open System.Collections.Generic
        open System.Reflection
        
        open FSharp.Quotations
        open FSharp.Quotations.DerivedPatterns
        open FSharp.Quotations.Patterns
        
        open ProviderImplementation.ProvidedTypes
        open ProviderImplementation.ProvidedTypes.UncheckedQuotations
        
        let cleanUpTypeName (str:string) =
            let sb = Text.StringBuilder(str)
            sb.Replace("System.", "")
              .Replace("Microsoft.FSharp.Core.", "")
              .Replace("Collections.Generic.","")
              .Replace("Falanx.JsonCodec.", "")
              .Replace("Newtonsoft.Json.Linq.", "") 
              .Replace("`2", "")
              .Replace("`1", "")
              .Replace("FSharp", "")
              .Replace("Int32", "int") |> ignore
            sb.ToString()
            
        let simpleTypeFormatter (a:IO.TextWriter) (b:Type) =
            b.ToString()
            |> cleanUpTypeName
            |> a.Write
            
        let sprintfsimpleTypeFormatter () (b:Type) =
            b.ToString()
            |> cleanUpTypeName
        
        let quotationsTypePrinter expr =
        
            let rec traverseQuotation f q = 
              let q = defaultArg (f q) q
              match q with     
              | ExprShape.ShapeLambda(v, body)  -> Expr.Lambda(v, traverseQuotation f body)
              | ExprShape.ShapeCombination(comb, args) -> ExprShape.RebuildShapeCombination(comb, List.map (traverseQuotation f) args )          
              | ExprShape.ShapeVar _ -> q
                 
            traverseQuotation (fun e -> match e with
                                        | Call(_,mi,args) when mi.IsGenericMethod -> 
                                           printfn "Call: %s" mi.Name
                                           printfn "    Args: %A" args
                                           printfn "    Type: %a" simpleTypeFormatter e.Type
                                           printfn "    MI_RType: %a" simpleTypeFormatter mi.ReturnType
                                           mi.GetGenericArguments()
                                           |> Array.iteri (fun i a -> printfn "    %i: %a" i simpleTypeFormatter a)
                                           printfn ""
                                           None
                                        | Let(v,mi,_) ->
                                            printfn "Let:\n    Var: %s\n    Type: %a\n" v.Name simpleTypeFormatter v.Type
                                            None
                                        | Lambda(v, expr) ->
                                           printfn "Lambda:"
                                           printfn "    Var: %s\n    Type: %a" v.Name simpleTypeFormatter v.Type
                                           printfn "    expr: %A" expr
                                           printfn "    Type: %a" simpleTypeFormatter expr.Type
                                           
                                           None
                                        | _ -> None) expr
        
        let x<'T> : 'T = Unchecked.defaultof<'T>
        let private onlyVar = function Var v -> Some v | _ -> None
        
        /// Get FieldInfo from expression
        let fieldof expr =
          match expr with
            | FieldGet(_, info) -> info
            | Lambda(arg, FieldGet(Some(Var var), info))
            | Let(arg, _, FieldGet(Some(Var var), info))
                when arg = var -> info
            | _ -> failwith "Not a field expression"
        
        /// Get PropertyInfo from expression
        let propertyof expr =
          match expr with
            | PropertyGet(_, info, _) -> info
            | Lambda(arg, PropertyGet(Some(Var var), info, _))
            | Let(arg, _, PropertyGet(Some(Var var), info, _))
                when arg = var -> info
            | _ -> failwith "Not a property expression"
        
        let private (|LetInCall|_|) expr =
            let rec loop e collectedArgs =
                match e with
                | Let(arg, _, exp2) ->  
                    let newArgs = Set.add arg collectedArgs
                    loop exp2 newArgs
                | Call(instance, mi, args) ->
                    let setOfCallArgs =
                        args
                        |> List.choose onlyVar
                        |> Set.ofList
                    if Set.isSubset setOfCallArgs collectedArgs then
                        Some mi
                    else None
                | _ -> None
            loop expr Set.empty
        
        let private (|Func| _ |) expr =
            
            match expr with
            // function values without arguments
            | Lambda (arg, Call (target, info, []))
                when arg.Type = typeof<unit> -> Some (target, info)
            // function values with one argument
            | Lambda (arg, Call (target, info, [Var var]))
                when arg = var -> Some (target, info)
            // function values with a set of curried or tuple arguments
            | Lambdas (args, Call (target, info, exprs)) ->
                let justArgs = List.choose onlyVar exprs
                let allArgs = List.concat args
                if justArgs = allArgs then
                    Some (target, info)
                else None
            | Lambdas(args, somethingElse) ->
                None
            | _ -> None
            
        /// Get a MethodInfo from an expression that is a method call or a function type value
        let rec methodof expr =
            match expr with
            // any ordinary calls: foo.Bar ()
            | Call (_, info, _) -> info
            // calls and function values via a lambda argument:
            // fun (x: string) -> x.Substring (1, 2)
            // fun (x: string) -> x.StartsWith
            | Lambda (arg, Call (Some (Var var), info, _))
            | Lambda (arg, Func (Some (Var var), info))
                when arg = var -> info
            // any function values:someString.StartsWith
            | Func (_, info) -> info
            // calls and function values ​​via instances:
            // "abc" .StartsWith ("a")
            // "abc" .Substring
            | Let (arg, _, Call (Some (Var var), info, _))
            | Let (arg, _, Func (Some (Var var), info))
                when arg = var -> info
            | LetInCall(info) -> info
            | _ -> failwith "Not a method expression"
            
        /// Gets the generic method definition of an expression
        /// which is a generic method, value expression orfunctional type
        let methoddefof expr =
          match methodof expr with
          | info when info.IsGenericMethod -> info.GetGenericMethodDefinition()
          | info -> failwithf "%A is not generic" info

        /// Gets the ConstructorInfo from a new object or record expression
        let constructorof expr =
            match expr with
            | NewObject (info, _) -> info
            // Get the record constructor
            | NewRecord (recordType, _) ->
                match recordType.GetConstructors() with
                | [| info |] -> info
                | _ -> failwith "Invalid record type"
            | _ -> failwith "Not a constructor expression"
        
        /// Obtaining CLI-compliant EventInfo from an expression
        let eventof expr =
            match expr with
            | Call(None, createEvent, [Lambda (arg1, Call (_, add, [Var var1]))
                                       Lambda (arg2, Call (_, remove, [Var var2]))
                                       Lambda (_, NewDelegate _)] )
                when createEvent.Name = "CreateEvent" && add.Name.StartsWith ("add_")
                                                      && remove.Name.StartsWith ("remove_")
                                                      && arg1 = var1 && arg2 = var2 ->
                    add.DeclaringType.GetEvent(add.Name.[0..4], BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.NonPublic)
            | _ -> failwith "Not a event expression"
        
        /// Get UnionCaseInfo from an expression
        let unioncaseof expr =
            match expr with
            | NewUnionCase (info, _)
            | UnionCaseTest (_, info) -> info
            | _ -> failwith "Not a union case expression"
        
        
        let sequence expressions = 
            if expressions |> Seq.isEmpty then Expr.Value(()) 
            else expressions |> Seq.reduce (fun acc s -> Expr.Sequential(acc, s))
               
        let private isGenerated (ty: Type) =
            ty :? ProvidedTypeDefinition || 
            (ty.IsGenericType && ty.GetGenericArguments() |> Seq.exists (fun gt -> gt :? ProvidedTypeDefinition))
        
        let makeGenericMethod (types: Type list) methodInfo =
            if types |> List.exists isGenerated
            then ProvidedTypeBuilder.MakeGenericMethod(methodInfo, types)
            else methodInfo.MakeGenericMethod(types |> Array.ofList)
        
        let makeGenericType (types: Type list) typeDef =
            if types |> List.exists isGenerated
            then ProvidedTypeBuilder.MakeGenericType(typeDef, types)
            else typeDef.MakeGenericType(types |> Array.ofList)
            
        let callStatic parameters staticMethod = Expr.CallUnchecked(staticMethod, parameters)
            
        let callStaticGeneric types arguments expr =
            expr |> methoddefof |> makeGenericMethod types |> callStatic arguments
    
        let rec private typeHierarchy (ty: Type) = seq {
            if not <| isNull ty
            then
                yield ty
                yield! typeHierarchy ty.BaseType
        }
        
        /// Generates an expression that iterates over a given sequence using provided body expression
        let forLoop (sequence: Expr) (body: Expr -> Expr) =
            let elementType = 
                typeHierarchy sequence.Type
                |> Seq.tryFind (fun ty -> ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<seq<_>>)
                |> Option.map (fun ty -> ty.GetGenericArguments().[0])
                |> function
                   | Some(x) -> x
                   | None -> failwith "Given collection is not a seq<'T>"
                
            let iterVar = Var("current", elementType)
            let enumeratorVar = Var("enumerator", typedefof<IEnumerator<_>> |> makeGenericType [elementType])
            let enumeratorExpr = Expr.Var enumeratorVar
            let moveNextMethod = typeof<IEnumerator>.GetMethod("MoveNext")
            let disposeMethod = typeof<IDisposable>.GetMethod("Dispose")
            
            let whileLoop = 
                Expr.WhileLoop(
                    Expr.Call(enumeratorExpr, moveNextMethod, []),
                    Expr.Let(
                        iterVar, 
                        Expr.PropertyGet(enumeratorExpr, enumeratorVar.Type.GetProperty("Current")), 
                        body <| Expr.Var iterVar))
            
            // Expr.TryFinally is not really supported by FSharp.TypeProviders.StarterPack
            // so, Expr.Sequential is used instead (Dispose() won't be called if exception is raised)
            Expr.Let(
                enumeratorVar, 
                Expr.Call(sequence, sequence.Type.GetMethod("GetEnumerator"), []),
                Expr.Sequential(whileLoop, Expr.Call(enumeratorExpr, disposeMethod, [])))
                  
        let equal (a: Expr) (b: Expr) =
            if a.Type = b.Type then
                callStaticGeneric [a.Type] [a; b] <@@ Unchecked.defaultof<_> = Unchecked.defaultof<_> @@>
            else
                Printf.ksprintf invalidOp "Arguments should have the same type, but their types: %s and %s" a.Type.FullName b.Type.FullName
                
        let defaultOf ty =
            callStaticGeneric [ty] [] <@@ Unchecked.defaultof<_> @@>
        
        let apply lambda =
            Seq.fold (fun l arg -> Expr.Application(l, arg)) lambda
        
        let box expr = Expr.Coerce(expr, typeof<obj>)
