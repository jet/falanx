namespace Falanx.Proto.Codec.Json
open System
open System.Reflection
open System.Collections.Generic
open Fleece
open Fleece.Newtonsoft
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Falanx.Machinery
open Falanx.Machinery.Expr
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open Falanx.Proto.Core.Model
open ProviderImplementation.ProvidedTypes
open Newtonsoft.Json.Linq
open Reflection
open Froto.Parser.ClassModel
     
module Codec =    
    let knownNamespaces = ["System"; "System.Collections.Generic"; "Fleece.Newtonsoft"; "Microsoft.FSharp.Core"; "Newtonsoft.Json.Linq"] |> Set.ofList
    let qs = QuotationSimplifier(true)                          
         
    let flatten = function None -> ResizeArray() | Some x -> x
    let expand (x: ResizeArray<_>) = if isNull x || Seq.isEmpty x then None else Some x
    
    let createLambdaRecord (typeDescriptor: TypeDescriptor) =
        //Lambda (u, Lambda (t, NewRecord (Result2, u, t))
        let recordType = typeDescriptor.Type :?> ProvidedRecord
        let recordFields = recordType.RecordFields
        let recordFields2 = typeDescriptor.Fields
        
        let recordVars =
            recordFields2
            |> List.map(function
                        | Property{PropertyDescriptor.Rule = ProtoFieldRule.Repeated; ProvidedProperty = pp} ->
                            //For fleece processing we need to wrap repeat field types in option
                            //We also add the flatten and expand methods in the to lambda construction and
                            //field map respectively.
                            Var(pp.Name, typedefof<Option<_>>.MakeGenericType(pp.PropertyType)), true
                        | Property{ProvidedProperty = pp} ->
                            Var(pp.Name, pp.PropertyType), false
                        | OneOf oneOf ->
                            Var(oneOf.CaseProperty.Name, oneOf.CaseProperty.PropertyType), false
                        | Map m ->
                            Var(m.ProvidedProperty.Name, m.ProvidedProperty.PropertyType), false )

        let mapRepeatOrStandard =
            fun (v, isRepeated) ->
                if isRepeated then
                    let v = Expr.Var v
                    let call = <@ flatten x @>
                    let mi = call |> function Call(_,mi,_) -> mi.GetGenericMethodDefinition() | _ -> failwith "not a call"
                    let miclosed = mi.MakeGenericMethod([|v.Type|])
                    Expr.CallUnchecked(miclosed, [v])
                    let exp = Expr.callStaticGeneric [v.Type] [v] call
                    exp
                else
                    Expr.Var v
 
        let result =
            List.foldBack (fun (v, isRepeated) acc -> Expr.Lambda(v,acc))
                recordVars (Expr.NewRecordUnchecked(recordType, recordVars |> List.map mapRepeatOrStandard))
        result
              
    let  getFunctionReturnType (typ: Type) =
        let rec loop (typ: Type) = 
            if FSharpTypeSafe.IsFunction typ then
                let domain, range = FSharpTypeSafe.GetFunctionElements typ                   
                loop range
            else typ  
        let result = loop typ
        result
        
    let rec getLambdaElements typ = [
        let returnOrLoop t = [
            if FSharpTypeSafe.IsFunction t then 
                yield! getLambdaElements t
            else yield t ]
        let domain, rest = FSharpTypeSafe.GetFunctionElements(typ)
        yield! returnOrLoop domain
        yield! returnOrLoop rest
        ]
        
    let makeFunctionTypeFromElements (xs: Type list) =
        let newFunction = xs |> List.reduceBack (fun a b -> FSharpType.MakeFunctionType(a, b) )
        newFunction
           
    let callMapping (lambda:Expr) =
    
        let replaceLambdaArgs (mi: MethodInfo) (lambda:Type) =
            let lambdaReturn = getFunctionReturnType lambda
            ProvidedTypeBuilder.MakeGenericMethod(mi, [lambda; typeof<IReadOnlyDictionary<string,JsonValue>>; typeof<string>; lambdaReturn; typeof<string>; typeof<JToken>] )

        //mapping f: 'a -> ('b -> Result<'a,'c>) * ('d -> IReadOnlyDictionary<'e,'f>)
        
        //As an expression it looks like:
        //Call (None, mapping, [Lambda (u, NewRecord (Result, u))])

        //in generic types or a C# point of view its defined:
        //Tuple<FSharpFunc<b, FSharpResult<a, c>>, FSharpFunc<d, IReadOnlyDictionary<e, f>>> mapping<a, b, c, d, e, f>(a f)

        //for a Record3 we would have a type signature like this:
        //mapping<Option<String> -> Option<String> -> Result3, IReadOnlyDictionary<String, JToken>, String, Result3, String, JToken>
        
        let mappingMethodInfo = Expr.methoddefof <@ withFields<_,IReadOnlyDictionary<string,JsonValue>,string,_,string,JToken> @>
        
        let lambdaType = lambda.Type
        
        let mappingMethodInfoFull = replaceLambdaArgs mappingMethodInfo lambdaType
        let mappingMiFullArgs = mappingMethodInfoFull.GetGenericArguments()
        
        //usage would normally be a lambda piped into mapping:
        //fun f : Option<String> -> Option<String> -> Result2 
        //|> mapping<Option<String> -> Option<String> -> Result2, IReadOnlyDictionary<String, JToken>, String, Result2, String, JToken> 
        
        //FSharpFunc`2[FSharpFunc`2[Option`1[String],FSharpFunc`2[Option`1[String],Result2]],Tuple`2[FSharpFunc`2[IReadOnlyDictionary`2[String,JToken],FSharpResult`2[FSharpFunc`2[FSharpOption`1[String],FSharpFunc`2[FSharpOption`1[String],Result2]],String]],FSharpFunc`2[IReadOnlyDictionary`2[String,NJToken]]]]

        //after having f applied to mapping:
        //(IReadOnlyDictionary<string,JsonValue> -> Result<(string option -> string option -> Result2),string>) * (Result2 -> IReadOnlyDictionary<string,JToken>
        
        //generic signature:
        //System.Tuple`2[FSharpFunc`2[IReadOnlyDictionary`2[String, JToken],FSharpResult`2[FSharpFunc`2[FSharpOption`1[String], FSharpFunc`2[FSharpOption`1[String],Result2]],String]],FSharpFunc`2[Result3,IReadOnlyDictionary`2[JToken]]]

        let f = Var("f", lambdaType)
        let call = Expr.CallUnchecked(mappingMethodInfoFull, [Expr.Var(f)])
        let lambda = Expr.Lambda(f, call)
        lambda        
        
    let callPipeRight (arg:Expr) (func:Expr) =
        let methodInfoGeneric = Expr.methoddefof<@ (|>) @>
        let funcTypeReturn = getFunctionReturnType func.Type
        let methodInfoTyped =
            ProvidedTypeBuilder.MakeGenericMethod(methodInfoGeneric, [arg.Type; funcTypeReturn])
        let expr = Expr.CallUnchecked(methodInfoTyped, [arg; func])
        expr
        

    //this needs to be done at the Expt -> AST level to remove the extra lambda call  
//    let callPipeRight2 (arg:Expr) (func:Expr) =
//        let methodInfoGeneric = Expr.methoddefof<@ (|>) @>
//        let funcTypeReturn = getFunctionReturnType func.Type
//
//        let methodInfoTyped = methodInfoGeneric.MakeGenericMethod([|arg.Type; funcTypeReturn|])
//        let expr = 
//            match func with 
//            | Lambda(_, Call(Some obj,minfo,args)) ->  Expr.Call(methodInfoTyped, [arg; Expr.Call(obj,minfo,args |> List.truncate (args.Length - 1))])
//            | Lambda(_, Call(None,minfo,args)) ->  Expr.CallUnchecked(methodInfoTyped, [arg; Expr.CallUnchecked(minfo,args |> List.truncate (args.Length - 1))])
//            | _ -> Expr.Call(methodInfoTyped, [arg; func])
//        expr

    // fun u t -> { url = u; title= t }
    // |> mapping<Option<String> -> Option<int> -> Result2, IReadOnlyDictionary<String, JToken>, String, Result2, String, JToken>
    // |> jfieldopt<Result2, string, Option<int> -> Result2> "url"   (fun x -> x.url)
    // |> jfieldopt<Result2, int   , Result2>                "title" (fun x -> x.title)   
    let callJfieldopt (recordType: Type) (propertyDescriptor: PropertyDescriptor) (fieldType: Type ) (nextFieldType: Type) =
        let jfieldoptMethodInfo = Expr.methoddefof <@ jfieldOpt<_,string,_> x x x @>

        let jFieldTypeArguments = [recordType; fieldType; nextFieldType]
        let jfieldoptMethodInfoTyped = ProvidedTypeBuilder.MakeGenericMethod(jfieldoptMethodInfo, jFieldTypeArguments)
        
        let fieldName = Expr.Value propertyDescriptor.ProvidedProperty.Name
        
        let xvar = Var("x", recordType)
        
        let getter =
            let property = Expr.PropertyGet( Expr.Var xvar, propertyDescriptor.ProvidedProperty)
            match propertyDescriptor.Rule with
            | ProtoFieldRule.Optional ->
                Expr.Lambda(xvar, property)
            | ProtoFieldRule.Repeated ->
                let call = <@ expand x @>
                let mi = call |> function Call(_,mi,_) -> mi.GetGenericMethodDefinition() | _ -> failwith "not a call"
                let miclosed = mi.MakeGenericMethod([|property.Type|])
                Expr.CallUnchecked(miclosed, [property])
                let exp = Expr.callStaticGeneric [property.Type] [property] call
                Expr.Lambda(xvar, exp)
            | ProtoFieldRule.Required ->
                failwith "ProtoFieldRule Required is not supported"

        // IReadOnlyDictionary<String, JToken> -> Result<Option<fieldType> -> Option<nextFieldType> -> Record, String>
        let domain = typeof<IReadOnlyDictionary<String, JToken>>
        let range =
            let currentField = typedefof<Option<_>>.MakeGenericType(fieldType)
            let functionType = FSharpType.MakeFunctionType(currentField, nextFieldType)
            
            //IReadOnlyDictionary<String,JToken> -> Result<Option<String> -> Option<Int32>   -> Result2, String>
            //IReadOnlyDictionary<String,JToken> -> Result<Option<Int32>                     -> Result2, String>
            typedefof<Result<_,_>>.MakeGenericType( [| functionType; typeof<string> |] )
            
        let decoderType = FSharpType.MakeFunctionType(domain, range)

        //decoder should be:
        //IReadOnlyDictionary<String,JToken> -> Result< Option<String> -> Option<Int32> -> Result2, String>
        //IReadOnlyDictionary<String,JToken> -> Result<Result2, String>
        let decoder = Var("decode", decoderType)
        
        // Record -> IReadOnlyDictionary<String, JToken>
        let encoderType = FSharpType.MakeFunctionType(recordType, typeof<IReadOnlyDictionary<String, JToken>>)
        let encoder = Var("encode", encoderType)
        
        // decoder * encoder
        // ( IReadOnlyDictionary<String, JToken> -> Result<Option<fieldType> -> Option<nextFieldType> -> Record, String>) * (Record -> IReadOnlyDictionary<String, JToken> )
        let codecType = FSharpType.MakeTupleType [|decoderType; encoderType|]
        let codec = Var("codec", codecType)
        
        //<*> jopt  "ids"  (fun x -> expand x.Ids)
        
        Expr.Lambda(codec,
            Expr.Let(decoder, Expr.TupleGet(Expr.Var codec, 0),
                Expr.Let(encoder, Expr.TupleGet(Expr.Var codec, 1),
                    Expr.CallUnchecked(jfieldoptMethodInfoTyped, [fieldName; getter; Expr.Var decoder; Expr.Var encoder]))))
        
    let callJfield (recordType: Type) (propertyDescriptor: PropertyDescriptor) (fieldType: Type ) (nextFieldType: Type) =
        let jfieldMethodInfo = Expr.methoddefof <@ jfield<_,string,_> x x x @>

        let jFieldTypeArguments = [recordType; fieldType; nextFieldType]
        let jfieldMethodInfoTyped = ProvidedTypeBuilder.MakeGenericMethod(jfieldMethodInfo, jFieldTypeArguments)
        
        let fieldName = Expr.Value propertyDescriptor.ProvidedProperty.Name
        
        let xvar = Var("x", recordType)
        
        let getter = Expr.Lambda(xvar, Expr.PropertyGet( Expr.Var xvar, propertyDescriptor.ProvidedProperty) )

        // IReadOnlyDictionary<String, JToken> -> Result<Option<fieldType> -> Option<nextFieldType> -> Record, String>
        let domain = typeof<IReadOnlyDictionary<String, JToken>>
        let range =
            let currentField = fieldType //typedefof<Option<_>>.MakeGenericType(fieldType)
            let functionType = FSharpType.MakeFunctionType(currentField, nextFieldType)
            
            //IReadOnlyDictionary<String,JToken> -> Result<Option<String> -> Option<Int32>   -> Result2, String>
            //IReadOnlyDictionary<String,JToken> -> Result<Option<Int32>                     -> Result2, String>
            typedefof<Result<_,_>>.MakeGenericType( [| functionType; typeof<string> |] )
            
        let decoderType = FSharpType.MakeFunctionType(domain, range)

        //decoder should be:
        //IReadOnlyDictionary<String,JToken> -> Result< Option<String> -> Option<Int32> -> Result2, String>
        //IReadOnlyDictionary<String,JToken> -> Result<Result2, String>
        let decoder = Var("decode", decoderType)
        
        // Record -> IReadOnlyDictionary<String, JToken>
        let encoderType = FSharpType.MakeFunctionType(recordType, typeof<IReadOnlyDictionary<String, JToken>>)
        let encoder = Var("encode", encoderType)
        
        // decoder * encoder
        // ( IReadOnlyDictionary<String, JToken> -> Result<Option<fieldType> -> Option<nextFieldType> -> Record, String>) * (Record -> IReadOnlyDictionary<String, JToken> )
        let codecType = FSharpType.MakeTupleType [|decoderType; encoderType|]
        let codec = Var("codec", codecType)
        
        Expr.Lambda(codec,
            Expr.Let(decoder, Expr.TupleGet(Expr.Var codec, 0),
                Expr.Let(encoder, Expr.TupleGet(Expr.Var codec, 1),
                    Expr.CallUnchecked(jfieldMethodInfoTyped, [fieldName; getter; Expr.Var decoder; Expr.Var encoder]))))
                        
    let createRecordJFieldOpts (typeDescriptor: TypeDescriptor) =
        let recordType = typeDescriptor.Type :> Type
        let recordFields = typeDescriptor.Properties
        let fieldTypeWithRest =
            let rec loop (recordFields: PropertyDescriptor list) =
                [
                    match recordFields with
                    | [] -> ()
                    | h :: [] ->
                        yield h, recordType
                    | h :: t ->
                        let rest = (t |> List.map (fun pt -> pt.ProvidedProperty.PropertyType)) @ [recordType]
                        let all = makeFunctionTypeFromElements rest
                        yield h, all
                        yield! loop t
                ]
            loop recordFields
  
        let optionType (o: Type) = o.GetGenericArguments().[0]
        
        let jFieldOptions =
            fieldTypeWithRest
            |> List.map (fun (propertyDescriptor, rest) ->
                match propertyDescriptor.Rule with
                | ProtoFieldRule.Optional ->
                    let fieldType = optionType propertyDescriptor.ProvidedProperty.PropertyType
                    callJfieldopt typeDescriptor.Type propertyDescriptor fieldType rest
                | ProtoFieldRule.Repeated ->
                    let fieldType = propertyDescriptor.ProvidedProperty.PropertyType
                    callJfieldopt typeDescriptor.Type propertyDescriptor fieldType rest
                | ProtoFieldRule.Required ->
                    failwith "not supported")        
        jFieldOptions
        
    let createJsonObjCodec (typeDescriptor: TypeDescriptor) =     
        let recordType = typeDescriptor.Type :?> ProvidedRecord
        let lambdaRecord = createLambdaRecord typeDescriptor
        let mapping = callMapping lambdaRecord
        let pipeLambdaToMapping = callPipeRight lambdaRecord mapping

        let jFieldOpts = createRecordJFieldOpts typeDescriptor
        
        let allPipedFunctions = [yield lambdaRecord; yield mapping; yield! jFieldOpts]
        let foldedFunctions = allPipedFunctions |> List.reduce callPipeRight                                   

        let ctast, ctpt = Quotations.ToAst( foldedFunctions, knownNamespaces = knownNamespaces )
 #if DEBUG
        let code = Fantomas.CodeFormatter.FormatAST(ctpt, "test", None, Fantomas.FormatConfig.FormatConfig.Default)
#endif                       

        let signatureType =
            let def = typedefof<Codec<IReadOnlyDictionary<string,JsonValue>,_>>
            def.MakeGenericType [|typeof<IReadOnlyDictionary<string,JsonValue>>; typeDescriptor.Type :> _ |]
            
        let createJsonObjCodec = ProvidedProperty("JsonObjCodec",signatureType, getterCode = (fun args -> foldedFunctions), isStatic = true )
        createJsonObjCodec
 
#if DEBUG       
[<CLIMutable>]
type Result =
    { mutable url : string option
      mutable title : string option
      mutable snippets : string ResizeArray }
    
    [<ReflectedDefinition>]
    static member JsonObjCodec =
        fun url title snippets -> { url = url; title = title; snippets = Codec.flatten snippets }
        |> withFields<Option<String> -> Option<String> -> Option<List<String>> -> Result, IReadOnlyDictionary<String, JToken>, DecodeError, Result, String, JToken>
        |> jfieldOpt "url" (fun x -> x.url)
        |> jfieldOpt "title"  (fun x -> x.title)
        |> jfieldOpt "snippets"  (fun x -> Codec.expand x.snippets)
#endif
