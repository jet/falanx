namespace Falanx.JsonCodec
open System
open Fleece
open Fleece.Newtonsoft
open Fleece.Newtonsoft.Operators
open Fleece.Newtonsoft.Helpers
open Fleece.Newtonsoft.ReadOnlyCollections
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns
open Falanx.Ast

[<CLIMutable>]
type Result2 =
    { mutable url : string option 
      mutable title : string option }
      
[<CLIMutable>]
type Result3 =
  { mutable url : string option
    mutable title : string option
    mutable snippets : string ResizeArray }

module Quotations =
    let rec traverseForCall q = [
      match q with
      | Patterns.Call _ as call -> yield call
      | Quotations.ExprShape.ShapeLambda(v, body)  -> yield! traverseForCall body
      | Quotations.ExprShape.ShapeCombination(comb, args) -> 
          for ex in args do yield! traverseForCall ex
      | Quotations.ExprShape.ShapeVar _ -> () ]

module temp =
    open System.Reflection
    open Newtonsoft.Json.Linq

    let x<'T> : 'T = Unchecked.defaultof<'T>
    let knownNamespaces = ["System"; "System.Collections.Generic"; "Fleece.Newtonsoft"; "Microsoft.FSharp.Core"; "Newtonsoft.Json.Linq"] |> Set.ofList
    let qs = ProviderImplementation.ProvidedTypes.QuotationSimplifier(true)
           
    let result2pipe =
        fun u t -> { url = u; title= t }
        |>mapping
        |> jfieldopt "url" (fun x -> x.url)
        |> jfieldopt "title" (fun x -> x.title)
        
    let result2apply =
        jfieldopt "title" (fun x -> x.title) 
            (jfieldopt "url" (fun x -> x.url)
                (mapping (fun u t -> { url = u; title= t })))
                
    let result2applyExpression =
        <@  jfieldopt "title" (fun x -> x.title) 
                (jfieldopt "url" (fun x -> x.url)
                    (mapping (fun u t -> { url = u; title= t }))) @>
           
    let result2Expression =
         <@ let fieldName1 = "url"
            let fieldName2 = "title"
            let getter1 = (fun x -> x.url)
            let getter2 = (fun x -> x.title)
            let mapper = mapping (fun u t -> { url = u; title = t })
            
            jfieldopt fieldName2 getter2
              (jfieldopt fieldName1 getter1
                  mapper) @>
                                    
    let result2Pipe =
        <@  fun u t -> { url = u; title= t }
            |> mapping
            |> jfieldopt "url" (fun x -> x.url)
            |> jfieldopt "title" (fun x -> x.title) @>
                   
    let result2PipeToMapping = 
        <@  fun u t -> { url = u; title= t }
            |> mapping<string option -> string option -> Result2,IReadOnlyDictionary<string,JsonValue>,string,Result2,string,JToken> @>
                   
    let result3Apply =
        jfield "snippets"  (fun x -> x.snippets)
            (jfieldopt "title"  (fun x -> x.title)
              (jfieldopt "url" (fun x -> x.url)
                  (
                      mapping (fun u t s -> { url = u; title = t; snippets = s })
                  )
              )
            )
            
    let result3ApplyExpr = <@
        jfield "snippets"  (fun x -> x.snippets)
            (jfieldopt "title"  (fun x -> x.title)
            (jfieldopt "url" (fun x -> x.url)
            (mapping (fun u t s -> { url = u; title = t; snippets = s })))) @>
                   
    let aa = fun u t -> { url = u; title= t }
    let bb = mapping<_,IReadOnlyDictionary<string,JsonValue>,string,_,string,JToken>
    let cc = jfieldopt "url" (fun x -> x.url)
    let dd = jfieldopt "title" (fun x -> x.title)
    let abcde = aa |> bb |> cc |> dd  
     
    let typeName (m : Type) = 
        m.Name.Split('`').[0]
    let rec generic (m : Type) = 
        if m.IsGenericType then   
            sprintf "typedefof<%s>.MakeGernericType(%s)" (typeName m) (m.GetGenericArguments() |> genericArgs)
            
        else
            sprintf "typeof<%s>" (typeName m)
    and genericArgs (args : Type []) = 
        let x = args |> Array.map generic |> String.concat ", "
        sprintf "[|%s|]" x
    
    let rec fsSig (t : Type) = 
        if Reflection.FSharpType.IsFunction t then 
            let a,b = Reflection.FSharpType.GetFunctionElements t
            sprintf "%s -> %s" (fsSig a) (fsSig b)
        elif Reflection.FSharpType.IsTuple t then 
            let types = Reflection.FSharpType.GetTupleElements t
            let str = types |> Array.map fsSig |> String.concat " * "
            sprintf "(%s)" str
        else    
            typeName t
                           
    let recordCreation() =
        // NewRecord (Result2, u, t)
        let recordType = typeof<Result2>
        Expr.NewRecord(recordType, [ <@ None: Option<string> @>; <@ None: Option<string> @> ])
        
    let lambdaRecord() =
        //Lambda (u, Lambda (t, NewRecord (Result2, u, t))
        let uType = typeof<string option>
        let tType = typeof<string option >
        let recordType = typeof<Result2>
        let l1 =
            let u = Var("u", uType)
            let t = Var("t", tType)
            Expr.Lambda(u,
                Expr.Lambda(t, 
                    Expr.NewRecord(recordType, [Expr.Var u; Expr.Var t] )))
        l1
      
    let replaceArgs (mi: MethodInfo) (a:Type) b =
        let genericDefinition = mi.GetGenericMethodDefinition()
        genericDefinition.MakeGenericMethod([|a; typeof<IReadOnlyDictionary<string,JsonValue>>; typeof<string> ;b; typeof<string>; typeof<JToken>|])
        
    let rec getFunctionReturnType typ =
        if FSharp.Reflection.FSharpType.IsFunction typ then
            let domain, range = FSharp.Reflection.FSharpType.GetFunctionElements typ
            getFunctionReturnType range
        else typ
        
    let rec getLambdaElements typ = [
        let returnOrLoop t = [
            if Reflection.FSharpType.IsFunction t then 
                yield! getLambdaElements t
            else yield t ]
        let domain, rest = Reflection.FSharpType.GetFunctionElements(typ)
        yield! returnOrLoop domain
        yield! returnOrLoop rest
        ]
        
    let makeFunctionTypeFromElements (xs: Type list) =
        let thing = xs |> List.reduceBack (fun a b -> Reflection.FSharpType.MakeFunctionType(a, b) )
        thing
        
    
    let callMapping() =
        //As an expression is looks like:
        //Call (None, mapping, [Lambda (u, NewRecord (Result, u))])

        //mapping takes:
        //f: 'a -> ('b -> Result<'a,'c>) * ('d -> IReadOnlyDictionary<'e,'f>)

        //in generic types its defined:
        //Tuple<FSharpFunc<b, FSharpResult<a, c>>, FSharpFunc<d, IReadOnlyDictionary<e, f>>> mapping<a, b, c, d, e, f>(a f)

        //for the prototype with Record3 we would have a type signature like this:
        //<Option<String> -> Option<String> -> Result3, IReadOnlyDictionary<String, JToken>, String, Result3, String, JToken>
        
        let mappingMi =
            <@ mapping<_,IReadOnlyDictionary<string,JsonValue>,string,_,string,JToken> @>
            |> function Lambda(_, Call(_,mi,_)) -> mi | _ -> failwith "failed!"
        
        //get lambdas type and return
        let lambda = lambdaRecord()
        let lambdaType = lambda.Type
        let lambdaReturn = getFunctionReturnType lambdaType
        
        let mappingMiFull = replaceArgs mappingMi lambdaType lambdaReturn
        let mappingMiFullArgs = mappingMiFull.GetGenericArguments()
        
        //usage would be like this normally in idiomatic F# (Type annotations added to gain clarity)
        //fun f : Option<String> -> Option<String> -> Result2 
        //|> mapping<Option<String> -> Option<String> -> Result2, IReadOnlyDictionary<String, JToken>, String, Result2, String, JToken> 
        
        //FSharpFunc`2[FSharpFunc`2[Option`1[String],FSharpFunc`2[Option`1[String],Result2]],Tuple`2[FSharpFunc`2[IReadOnlyDictionary`2[String,JToken],FSharpResult`2[FSharpFunc`2[FSharpOption`1[String],FSharpFunc`2[FSharpOption`1[String],Result2]],String]],FSharpFunc`2[IReadOnlyDictionary`2[String,NJToken]]]]

        //after having f applied to mapping:
        //(IReadOnlyDictionary<string,JsonValue> -> Result<(string option -> string option -> Result2),string>) * (Result2 -> IReadOnlyDictionary<string,JToken>
        
        //generic signature:
        //System.Tuple`2[FSharpFunc`2[IReadOnlyDictionary`2[String, JToken],FSharpResult`2[FSharpFunc`2[FSharpOption`1[String], FSharpFunc`2[FSharpOption`1[String],Result2]],String]],FSharpFunc`2[Result3,IReadOnlyDictionary`2[JToken]]]

        let lambdaElements = getLambdaElements lambdaType
        let lambdaPlusMapping = makeFunctionTypeFromElements [yield! lambdaElements ; yield mappingMiFull.ReturnType ]

        let f = Var("f", lambdaType)
        let call = Expr.Call(mappingMiFull, [Expr.Var(f)])
        let lambda = Expr.Lambda(f, call)
        lambda
        
    let callPipeRight (arg:Expr) (func:Expr) =
        //public static TResult op_PipeRight<T1, TResult>(T1 arg, FSharpFunc<T1, TResult> func)
        let pipeRight = <@ (|>) @>
        //first param:
        //FSharpFunc`2[FSharpOption`1[String],FSharpFunc`2[FSharpOption`1[String],Result2]]
        //second param:
        //FSharpFunc`2[FSharpFunc`2[FSharpOption`1[String],FSharpFunc`2[FSharpOption`1[String],Result2]],Tuple`2[FSharpFunc`2[IReadOnlyDictionary`2[JToken],FSharpResult`2[FSharpFunc`2[FSharpOption`1[String],FSharpFunc`2[FSharpOption`1[String],Result2]],String]],FSharpFunc`2[Result2,IReadOnlyDictionary`2[String,JToken]]]]
        let fullPipeRight = <@ fun (u : Option<String>) (t : Option<String>) -> { url = u; title = t } : Result2
                               |> mapping<Option<String> -> Option<String> -> Result2, IReadOnlyDictionary<String, JToken>, String, Result2, String, JToken> @>
        let fullPipeRightCall =  fullPipeRight |> function Call(_,mi,_) -> mi | _ -> failwith "failed!"
        //let fullPipeRightCallType = fullPipeRightCall.Type              
        let methodInfoUnTyped = pipeRight |> function Lambda(_, Lambda(_, Call(_,mi,_))) -> mi | _ -> failwith "failed!"
        let methodInfoGeneric = methodInfoUnTyped.GetGenericMethodDefinition()
        
        let funcType = func.Type
        let argType = arg.Type
        
        let methodInfoTyped   = methodInfoGeneric.MakeGenericMethod([|func.Type; arg.Type|])        
        let expr = Expr.Call(methodInfoTyped, [func; arg])
        expr
    

    let ttt : (IReadOnlyDictionary<String,JToken> -> Result<(Option<String> -> Option<String> -> Result2),String>) * (Result2 -> IReadOnlyDictionary<String,JToken>) =
        let a = fun (u : Option<String>) (t : Option<String>) -> { url = u; title = t } : Result2
        let b = mapping<Option<String> -> Option<String> -> Result2, IReadOnlyDictionary<String, JToken>, String, Result2, String, JToken>        
        a |> b
     
    let printerOfDoom() =   
        let q = 
            <@ 
                        let a = fun (u : Option<String>) (t : Option<String>) -> { url = u; title = t } : Result2
                        let b = mapping<Option<String> -> Option<String> -> Result2, IReadOnlyDictionary<String, JToken>, String, Result2, String, JToken>        
                        a |> b
            @>
        
        let rec loop q =
            match q with
            | Patterns.Let(_,_,b) -> loop b
            | Patterns.Call(_,m,_) when m.IsGenericMethod ->
                m.GetGenericArguments()
            
        printfn "%A" (loop q)

    let tryCode() =
        printerOfDoom()
        let result2Pipe = result2Pipe
        let result2PipeToMapping = result2PipeToMapping
        let result2Apply = result2applyExpression
        let result3Apply = result3ApplyExpr

        let ttt1, ttt2 = ttt

        let record = recordCreation()
        let lambdaRecord = lambdaRecord()  
        let mapping = callMapping()
        let pipecreationToMapping = callPipeRight lambdaRecord mapping
        let ctast, ctpt = Quotations.ToAst( mapping, knownNamespaces = knownNamespaces )
        let code = Fantomas.CodeFormatter.FormatAST(ctpt, "test", None, Fantomas.FormatConfig.FormatConfig.Default)
        code
        

        
(*
Call (None, op_PipeRight,
      [Call (None, op_PipeRight,
             [Call (None, op_PipeRight, [Lambda (u, Lambda (t, NewRecord (Result2, u, t))), Lambda (f, Call (None, mapping, [f]))]),
              Let (fieldName, Value ("url"),
                   Let (getter, Lambda (x, PropertyGet (Some (x), url, [])),
                        Lambda (tupledArg,
                                Let (arg20@, TupleGet (tupledArg, 0),
                                     Let (arg21@, TupleGet (tupledArg, 1),
                                          Call (None, jfieldopt, [fieldName, getter, arg20@, arg21@] )
                                         )
                                    )
                               )
                       )
                  )
             ]
            ),
       Let (fieldName, Value ("title"),
            Let (getter, Lambda (x, PropertyGet (Some (x), title, [])),
                 Lambda (tupledArg,
                         Let (arg20@, TupleGet (tupledArg, 0),
                              Let (arg21@, TupleGet (tupledArg, 1),
                                   Call (None, jfieldopt, [fieldName, getter, arg20@, arg21@] )
                                  )
                             )
                        )
                )
           )
      ])
*)                                
    
//    let test = <@ jfieldopt<Result,string,_> x x @>
//    let testS = qs.SimplifyQuotation test
//    let simplified = qs.TranslateExpression testS
//    let ctast, ctpt = Quotations.ToAst(simplified, knownNamespaces = knownNamespaces )
//    let code = Fantomas.CodeFormatter.FormatAST(ctpt, "test", None, Fantomas.FormatConfig.FormatConfig.Default)

       
module test =


    let test() =

        //let original = {url = Some "John"}
        //let serialized = sprintf "%s" (string (toJson original))
        //let deserialized = parseJson<Result> serialized
        //original, serialized, deserialized
        ()
