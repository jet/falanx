namespace Falanx.Proto.Core
open System
open System.Reflection
open System.Collections.Generic
open Fleece
open Fleece.Newtonsoft
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open Falanx.Machinery
open Falanx.Machinery.Expr
open Falanx.Proto.Core.Model
open Newtonsoft.Json.Linq
open Reflection
open Froto.Parser.ClassModel
open Falanx.Proto.Codec.Json.ResizeArray

#nowarn "686"   
module JsonCodec =
    let knownNamespaces = ["System"; "System.Collections.Generic"; "Fleece.Newtonsoft"; "Microsoft.FSharp.Core"; "Newtonsoft.Json.Linq"] |> Set.ofList
    let qs = QuotationSimplifier(true)                          
             
    let createLambdaRecord (typeDescriptor: TypeDescriptor) =
        //Lambda (u, Lambda (t, NewRecord (Result2, u, t))
        let recordType = typeDescriptor.Type :?> ProvidedRecord
        let recordFields = recordType.RecordFields
        let recordFields2 = typeDescriptor.Fields
        
        let recordVars =
            recordFields2
            |> List.map(function
                        | Property{PropertyDescriptor.Rule = ProtoFieldRule.Repeated; ProvidedProperty = pp} ->
                            //For fleece we need to wrap repeat field types in option.  We also add the flatten
                            //and expand methods in the to lambda construction and field map respectively.
                            Var(pp.Name, typedefof<Option<_>>.MakeGenericType(pp.PropertyType)) , Some(pp.PropertyType.GenericTypeArguments.[0])
                        | Property{ProvidedProperty = pp} ->
                            Var(pp.Name, pp.PropertyType), None
                        | OneOf oneOf ->
                            Var(oneOf.CaseProperty.Name, oneOf.CaseProperty.PropertyType), None
                        | Map m ->
                            Var(m.ProvidedProperty.Name, m.ProvidedProperty.PropertyType), None )

        let mapRepeatOrStandard =
            fun (v, maybeFlattened) ->
                match v, maybeFlattened with
                | v, Some flattened ->
                    let v = Expr.Var v
                    let exp = Expr.callStaticGeneric [flattened] [v] <@ flatten x @>
                    exp
                | v, None -> Expr.Var v
 
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
           
    let callwithFields (lambda:Expr) =
    
        let replaceLambdaArgs (mi: MethodInfo) (lambda:Type) =
            let lambdaReturn = getFunctionReturnType lambda
            ProvidedTypeBuilder.MakeGenericMethod(mi, [lambda; typeof<IReadOnlyDictionary<string,JsonValue>>; typeof<DecodeError>; lambdaReturn; typeof<string>; typeof<JToken>] )

        //mapping f: 'a -> ('b -> Result<'a,'c>) * ('d -> IReadOnlyDictionary<'e,'f>)
        
        //As an expression it looks like:
        //Call (None, mapping, [Lambda (u, NewRecord (Result, u))])

        //in generic types or a C# point of view its defined:
        //Tuple<FSharpFunc<b, FSharpResult<a, c>>, FSharpFunc<d, IReadOnlyDictionary<e, f>>> mapping<a, b, c, d, e, f>(a f)

        //for a Record3 we would have a type signature like this:
        //mapping<Option<String> -> Option<String> -> Result3, IReadOnlyDictionary<String, JToken>, String, Result3, String, JToken>
        
        let mappingMethodInfo = Expr.methoddefof <@ withFields<_,IReadOnlyDictionary<string, JsonValue>, DecodeError, _, string, JToken> @>
        
        let lambdaType = lambda.Type
        
        let mappingMethodInfoFull = replaceLambdaArgs mappingMethodInfo lambdaType
        let mappingMiFullArgs = mappingMethodInfoFull.GetGenericArguments()
        
        //usage would normally be a lambda piped into mapping:
        //fun f : Option<String> -> Option<String> -> Result2 
        //|> withFields<Option<String> -> Option<String> -> Result2, IReadOnlyDictionary<String, JToken>, String, Result2, String, JToken> 
        
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
                let exp = Expr.callStaticGeneric [yield! property.Type.GenericTypeArguments] [property] <@ expand x @>
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
            typedefof<Result<_,_>>.MakeGenericType( [| functionType; typeof<DecodeError> |] )
            
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
    
    let callmap (descriptor: OneOfDescriptor) (property: PropertyDescriptor) =
        let mapMi = Expr.methoddefof <@ FSharpPlus.Operators.map<int, string, int [], string []> x x @>
        
        let unionCase =
            descriptor.OneOfType
            |> ProvidedUnion.tryGetUnionCaseByPosition (int property.Position)
            
            
        let propertyType = property.Type.RuntimeType
        let unionType = descriptor.OneOfType :> Type
            
        let f1 =
        //ConcreteCodec<KeyValuePair<string, JsonValue> list, KeyValuePair<string, JsonValue> list, propertyType, unionType>
            let cc = typedefof<ConcreteCodec<_,_,_,_>>
            let keyPairs = typeof<KeyValuePair<string, JsonValue> list>
            ProvidedTypeBuilder.MakeGenericType(cc, [keyPairs; keyPairs; propertyType; unionType])

        let f2 =
        //ConcreteCodec<KeyValuePair<string, JToken> list, KeyValuePair<string, JToken> list, unionType, unionType>
            let cc = typedefof<ConcreteCodec<_,_,_,_>>
            let keyPairs = typeof<KeyValuePair<string, JToken> list>
            ProvidedTypeBuilder.MakeGenericType(cc, [keyPairs; keyPairs; unionType; unionType])
        
        let mapMiArguments = [propertyType; unionType; f1; f2]
        let mapMiTyped = ProvidedTypeBuilder.MakeGenericMethod(mapMi, mapMiArguments)

        let parameter1 =
        //First_name
        //Lambda (arg0, NewUnionCase (First_name, arg0))
            match unionCase with 
            | Some puc ->
                let v = Var("arg0", propertyType)
                let unionCaseInfo = Utils.mkUnionCaseInfo puc
                let lamb = Expr.Lambda(v, Expr.NewUnionCaseUnchecked(unionCaseInfo, [Expr.Var(v)] ))
                lamb
            | None ->
                failwithf "A union case was not found for: %s" property.ProvidedProperty.Name
        let parameter2 =
        //(Operators.jreq "First_name" (function First_name x -> Some x | _ -> None) )
            callJfield  
            <@@ () @@>
        let expr = Expr.CallUnchecked(mapMiTyped, [parameter1; parameter2])
        expr
        
    let calljchoice =
        let jchoiceMethodInfo = Expr.methoddefof <@ Newtonsoft.Operators.jchoice<seq<_>,_,_> x @>
        <@@ () @@>
        
    let createJsonObjCodecFromoneOf (descriptor: OneOfDescriptor) =
        let maps =
            descriptor.Properties
            |> Map.toList
            |> List.map (fun (_i, propertyDescriptor) -> callmap descriptor propertyDescriptor)
            
//        static member JsonObjCodec : ConcreteCodec<list<KeyValuePair<string, JToken>>, list<KeyValuePair<string, JToken>>, test_oneof, test_oneof> =
//            Operators.jchoice<list<KeyValuePair<string,JToken>>, test_oneof, test_oneof>
//                [
//                    FSharpPlus.Operators.map<string,
//                                             test_oneof,
//                                             ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,string,test_oneof>,
//                                             ConcreteCodec<KeyValuePair<string,JToken> list, KeyValuePair<string,JToken> list,test_oneof,test_oneof> >
//                                                 First_name (Operators.jreq "First_name" (function First_name x -> Some x | _ -> None))
//                    FSharpPlus.Operators.map<int,
//                                             test_oneof,
//                                             ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,int,test_oneof>,
//                                             ConcreteCodec<KeyValuePair<string,JToken> list,KeyValuePair<string,JToken> list,test_oneof,test_oneof>>
//                                                 Age (Operators.jreq "age" (function Age x -> Some x | _ -> None))
//                    FSharpPlus.Operators.map<string,
//                                             test_oneof,
//                                             ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,string,test_oneof>,
//                                             ConcreteCodec<KeyValuePair<string,JToken> list,KeyValuePair<string,JToken> list,test_oneof,test_oneof>>
//                                                 Last_name (Operators.jreq "last_name" (function Last_name x -> Some (x) | _ -> None))
//                ]
        ()
                        
    let createRecordJFieldOpts (typeDescriptor: TypeDescriptor) =
        let recordType = typeDescriptor.Type :> Type
        let recordFields = typeDescriptor.Properties
        
        let optionType (o: Type) = o.GetGenericArguments().[0]
        
        let createJfieldopt (propertyDescriptor: PropertyDescriptor) rest =
            match propertyDescriptor.Rule with
            | ProtoFieldRule.Optional ->
                let fieldType = optionType propertyDescriptor.ProvidedProperty.PropertyType
                callJfieldopt typeDescriptor.Type propertyDescriptor fieldType rest
            | ProtoFieldRule.Repeated -> //will call expand, so type signature affected
                
                let fieldType = propertyDescriptor.ProvidedProperty.PropertyType
                callJfieldopt typeDescriptor.Type propertyDescriptor fieldType rest
            | ProtoFieldRule.Required ->
                failwith "not supported"

        let fieldTypeWithRest =
            let rec loop (recordFields: PropertyDescriptor list) =
                [
                    match recordFields with
                    | [] -> ()
                    | head :: [] ->
                        yield createJfieldopt head recordType
                    | head :: tail ->
                        let rest =
                            tail
                            |> List.map (fun pd -> match pd.Rule with
                                                   | ProtoFieldRule.Repeated ->
                                                       typedefof<Option<_>>.MakeGenericType(pd.ProvidedProperty.PropertyType)
                                                   | _ ->
                                                       pd.ProvidedProperty.PropertyType)
                            
                        let restAsType = makeFunctionTypeFromElements (rest @ [recordType])
                        yield createJfieldopt head restAsType
                        yield! loop tail
                ]
            loop recordFields
        fieldTypeWithRest
        
    let createJsonObjCodec (typeDescriptor: TypeDescriptor) =     
        let recordType = typeDescriptor.Type :?> ProvidedRecord
        let lambdaRecord = createLambdaRecord typeDescriptor
        let mapping = callwithFields lambdaRecord
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
#nowarn "686"
[<CLIMutable>]
type TestAllTypes =
    { mutable singleInt32 : int option
      mutable repeatedString : string ResizeArray }
    
    [<ReflectedDefinition>]
    static member JsonObjCodec =
        fun singleInt32 repeatedString -> {singleInt32 = singleInt32; repeatedString = flatten<string> repeatedString}
        |> withFields<int option -> Option<ResizeArray<String>> -> TestAllTypes, IReadOnlyDictionary<String, JToken>, DecodeError, TestAllTypes, String, JToken>
        |> jfieldOpt<TestAllTypes, Int32, Option<ResizeArray<String>> -> TestAllTypes> "singleInt32"  (fun x -> x.singleInt32)
        |> jfieldOpt<TestAllTypes, ResizeArray<String> , TestAllTypes> "repeatedString"  (fun x -> expand<string> x.repeatedString)

        
type test_oneof =
    | First_name of First_name : string
    | Age of Age : int
    | Last_name of Last_name : string
    with
        [<ReflectedDefinition>]
        static member JsonObjCodec : ConcreteCodec<list<KeyValuePair<string, JToken>>, list<KeyValuePair<string, JToken>>, test_oneof, test_oneof> =
            Operators.jchoice<list<KeyValuePair<string,JToken>>, test_oneof, test_oneof>
                [|
                    FSharpPlus.Operators.map<string,
                                             test_oneof,
                                             ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,string,test_oneof>,
                                             ConcreteCodec<KeyValuePair<string,JToken> list, KeyValuePair<string,JToken> list,test_oneof,test_oneof> >
                                                 First_name (Operators.jreq "First_name" (function First_name x -> Some x | _ -> None))
                    FSharpPlus.Operators.map<int,
                                             test_oneof,
                                             ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,int,test_oneof>,
                                             ConcreteCodec<KeyValuePair<string,JToken> list,KeyValuePair<string,JToken> list,test_oneof,test_oneof>>
                                                 Age (Operators.jreq "age" (function Age x -> Some x | _ -> None))
                    FSharpPlus.Operators.map<string,
                                             test_oneof,
                                             ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,string,test_oneof>,
                                             ConcreteCodec<KeyValuePair<string,JToken> list,KeyValuePair<string,JToken> list,test_oneof,test_oneof>>
                                                 Last_name (Operators.jreq "last_name" (function Last_name x -> Some (x) | _ -> None))
                |]
                
        static member PrintDebug() =
            Expr.propertyof <@ test_oneof.JsonObjCodec @>
            |> function x -> x.GetMethod
            |> Expr.TryGetReflectedDefinition
            |> Option.iter (Expr.quotationsTypePrinter >> ignore)
#endif
