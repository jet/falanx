namespace Falanx.Ast
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
        
        let private (|Func| _ |) expr =
            let onlyVar = function Var v -> Some v | _ -> None
            match expr with
            // function values without arguments
            | Lambda (arg, Call (target, info, []))
                when arg.Type = typeof<unit> -> Some (target, info)
            // function values with one argument
            | Lambda (arg, Call (target, info, [Var var]))
                when arg = var -> Some (target, info)
            // function values with a set of curried or tuple arguments
            | Lambdas (args, Call (target, info, exprs))
                when List.choose onlyVar exprs
                    = List.concat args -> Some (target, info)
            | _ -> None
            
        /// Get a MethodInfo from an expression yhat is a method call or a function type value
        let methodof expr =
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
            | Call(None, createEvent, [Lambda (arg1, Call (_, addHandler, [Var var1]))
                                       Lambda (arg2, Call (_, removeHandler, [Var var2]))
                                       Lambda (_, NewDelegate _)] )
                when createEvent.Name = "CreateEvent"
                && addHandler.Name.StartsWith ("add_")
                && removeHandler.Name.StartsWith ("remove_")
                && arg1 = var1
                && arg2 = var2 ->
                    addHandler.DeclaringType.GetEvent(
                    addHandler.Name.Remove (0, 4),
                    BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.NonPublic)
            
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