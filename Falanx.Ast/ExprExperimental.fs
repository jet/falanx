namespace Falanx.Ast
    module ExprExperimental =
        open System
        open Microsoft.FSharp.Quotations
        open Microsoft.FSharp.Quotations.Patterns
        open Microsoft.FSharp.Quotations.DerivedPatterns
        open Microsoft.FSharp.Core.CompilerServices
        open Microsoft.FSharp.Reflection
        open ProviderImplementation.ProvidedTypes
        open ProviderImplementation.ProvidedTypes
        open ProviderImplementation.ProvidedTypes
        module Codec =
            let writeOption one two three four =
                ()
        let x<'T>  : 'T = Unchecked.defaultof<'T>      
        let x1<'T> : 'T = Unchecked.defaultof<'T>
        let x2<'T> : 'T = Unchecked.defaultof<'T>
        let x3<'T> : 'T = Unchecked.defaultof<'T>
        let x4<'T> : 'T = Unchecked.defaultof<'T>
             
        let rec traverseQuotation f (m: (Expr * Type option) array) q = 
          let q = defaultArg (f q) q
          match q with     
          | ShapeLambdaUnchecked(v, body)  -> Expr.Lambda(v, traverseQuotation f m body)
          | ShapeCombinationUnchecked(comb, args) -> RebuildShapeCombinationUnchecked(comb, List.map (traverseQuotation f m) args )          
          | ShapeVarUnchecked _ -> q
           
        let testTraverse underlyingType e (parameters: (Expr * Type option) array) =        
            let ee = e |> traverseQuotation (fun q ->
                match q with
                | Quotations.DerivedPatterns.SpecificCall <@ x1 @> (a,[ty],b) ->
                    let exp, t = parameters.[0]
                    let exp =
                        match t with
                        | Some t ->
                            match <@ Expr.Cast @> with 
                            | Call(inst, mi, parameters) ->
                                let newMi = mi.GetGenericMethodDefinition().MakeGenericMethod(t)
                                match inst with 
                                | Some i -> Expr.CallUnchecked(i, newMi, parameters)
                                | None -> Expr.CallUnchecked(newMi, parameters)
                            | _ -> failwith "Call not found"
                        | _ -> exp
                    Some exp
                | Quotations.DerivedPatterns.SpecificCall <@ x2 @> (_,[ty],_)  ->
                    let exp, t = parameters.[1]
                    let exp =
                        match t with
                        | Some t ->
                            match <@ Expr.Cast @> with 
                            | Call(inst, mi, parameters) ->
                                let newMi = mi.GetGenericMethodDefinition().MakeGenericMethod(t)
                                match inst with 
                                | Some i -> Expr.CallUnchecked(i, newMi, parameters)
                                | None -> Expr.CallUnchecked(newMi, parameters)
                            | _ -> failwith "Call not found"
                        | _ -> exp
                    Some exp
                | Quotations.DerivedPatterns.SpecificCall <@ x3 @> (_,[ty],_)  ->
                    let exp, t = parameters.[2]
                    let exp =
                        match t with
                        | Some t ->
                            match <@ Expr.Cast @> with 
                            | Call(inst, mi, parameters) ->
                                let newMi = mi.GetGenericMethodDefinition().MakeGenericMethod(t)
                                match inst with 
                                | Some i -> Expr.CallUnchecked(i, newMi, parameters)
                                | None -> Expr.CallUnchecked(newMi, parameters)
                            | _ -> failwith "Call not found"
                        | _ -> exp
                    Some exp
                | Quotations.DerivedPatterns.SpecificCall <@ x4 @> (_,[ty],_)  ->
                    let exp, t = parameters.[3]
                    let exp =
                        match t with
                        | Some t ->
                            match <@ Expr.Cast x @> with 
                            | Call(inst, mi, parameters) ->
                                let newMi = mi.GetGenericMethodDefinition().MakeGenericMethod(t)
                                match inst with 
                                | Some i -> Expr.CallUnchecked(i, newMi, [exp])
                                | None -> Expr.CallUnchecked(newMi, [exp])
                            | _ -> failwith "Call not found"
                        | _ -> exp
                    Some exp
                | Call(inst, mi, parameters) ->
                    let newMi = mi.GetGenericMethodDefinition().MakeGenericMethod(underlyingType)
                    match inst with 
                    | Some i -> Some <| Expr.CallUnchecked(i, newMi, parameters)
                    | None -> Some <| Expr.CallUnchecked(newMi, parameters)
                | other -> 
                    printfn "%A" other
                    None) parameters
            ee
            
    //    type Expr with
    //        static member callStaticGeneric2(types, arguments :Expr list, [<ReflectedDefinition>] expr: Expr<_>) =
    //            expr
    //            |> getMethodDef
    //            |> makeGenericMethod types
    //            |> callStatic arguments
    
    //    type one<'a> = 'a
    //    type two<'a,'b> = 'a -> 'b
    //    type three<'a,'b,'c> = 'a -> 'b -> 'c
    //    type writer<'a,'b,'c,'d> = 'b -> 'c -> 'a -> 'd
    //    type fourparams<'a,'b,'c> = (Writer<'a>) -> 'b -> 'c -> 'a option -> unit
    //      
    //    type Parameters<'a, 'b, 'c, 'd> =
    //        | CallOne of one<'a> * Expr
    //        | CallTwo of two<'a,'b> * Expr * Expr
    //        | CallThree of three<'a,'b,'c> * Expr * Expr * Expr
    //        | CallFour of fourparams<'a,'b,'c> * Expr * Expr * Expr * Expr
    //
    //        
    //    let callExpressionWithParameters types (parameters: Parameters<_,_,_,_>) =
    //        let expr =
    //            match parameters with 
    //            | CallOne(e,a) ->
    //                Expr.callStaticGeneric types [a] <@@ e x @@>
    //            | CallTwo(e,a,b) ->
    //                Expr.callStaticGeneric types [a; b] <@@ e x x @@>
    //            | CallThree(e,a,b,c) ->
    //                Expr.callStaticGeneric types [a; b; c] <@@ e x x x @@>
    //            | CallFour(e,a,b,c,d) ->
    //                let expression = <@@ e @@>
    //                Expr.callStaticGeneric types [a; b; c; d] expression
    //            | _ -> failwith "Not implemented"
    //        expr
