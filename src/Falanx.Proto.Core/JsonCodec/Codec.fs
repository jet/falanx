namespace Falanx.Proto.Core
open System
open System.Reflection
open System.Collections.Generic
open Fleece
open Fleece.Newtonsoft
open FSharpPlus
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

[<AutoOpen>]
module FleeceExtensions =

    type ConcreteCodec<'S1, 'S2, 't1, 't2> with
        static member inline map  (field: ConcreteCodec<'S, 'S, 'f, 'T>) (f) =
            f <!> field
        
        static member inline apply  (currentField: ConcreteCodec<'S, 'S, 'f, 'T>) (remainderFields: ConcreteCodec<'S, 'S, 'f ->'r, 'T>) =
            remainderFields <*> currentField
            
#nowarn "686"   
module JsonCodec =
    let qs = QuotationSimplifier.QuotationSimplifier(true)                          
             
    let createLambdaRecord (typeDescriptor: TypeDescriptor) =
        //Lambda (u, Lambda (t, NewRecord (Result2, u, t))
        let recordType = typeDescriptor.Type :?> ProvidedRecord
        let recordFields = typeDescriptor.Fields
        
        let lambdaVars =
            recordFields
            |> List.map(function
                        | Property{PropertyDescriptor.Rule = ProtoFieldRule.Repeated; ProvidedProperty = pp} ->
                            //For fleece we need to wrap repeat field types in option.  
                            Var(pp.Name, typedefof<Option<_>>.MakeGenericType(pp.PropertyType))
                        | Property{ProvidedProperty = pp} ->
                            Var(pp.Name, pp.PropertyType)
                        | OneOf oneOf ->
                            Var(oneOf.CaseProperty.Name, oneOf.CaseProperty.PropertyType)
                        | Map m ->
                            Var(m.ProvidedProperty.Name, m.ProvidedProperty.PropertyType))

        let recordArguments =
            lambdaVars
            |> List.zip recordFields 
            |> List.map(function
                        | (Property{PropertyDescriptor.Rule = ProtoFieldRule.Repeated; ProvidedProperty = pp}, v) ->
                            //For fleece we need to flatten and expand methods in the to lambda construction and field map respectively.
                            Expr.callStaticGeneric [pp.PropertyType.GenericTypeArguments.[0] ] [Expr.Var v] <@ flatten x @>
                        | _, v -> Expr.Var v )
            
 
        let result =
            let state = Expr.NewRecordUnchecked(recordType, recordArguments)
            List.foldBack (fun var acc ->
                                            Expr.Lambda(var, acc)) lambdaVars state
            
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
        

//TODO: this needs to be done at the Expr -> AST level to remove the extra lambda call  
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
    let callJfieldopt (recordType: Type) protoFieldRule (providedProperty: ProvidedProperty) (fieldType: Type ) (nextFieldType: Type) =
        let jfieldoptMethodInfo = Expr.methoddefof <@ jfieldOpt<_,string,_> x x x @>

        let jFieldTypeArguments = [recordType; fieldType; nextFieldType]
        let jfieldoptMethodInfoTyped = ProvidedTypeBuilder.MakeGenericMethod(jfieldoptMethodInfo, jFieldTypeArguments)
        
        let fieldName = Expr.Value providedProperty.Name
        
        let xvar = Var("x", recordType)
        
        let getter =
            let property = Expr.PropertyGet( Expr.Var xvar, providedProperty)
            match protoFieldRule with
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
        //this can be simplified to:
        //Expr.Lambda(codec, Expr.CallUnchecked(jfieldoptMethodInfoTyped, [fieldName; getter; Expr.TupleGet(Expr.Var codec, 0) ; Expr.TupleGet(Expr.Var codec, 1)]))
        //but fantomas breaks on this
        
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
    
    let callJReq (descriptor: OneOfDescriptor) (property: PropertyDescriptor) fieldName codec =
        //(Operators.jreq "First_name" (function First_name x -> Some x | _ -> None))
        let jReqMi = Expr.methoddefof <@ Operators.jreq<string,string> x x @>
        let propertyType = property.Type.RuntimeType
        let unionType = descriptor.OneOfType :> Type
        
        let jReqArguments = [unionType; propertyType]
        let jReqTyped = ProvidedTypeBuilder.MakeGenericMethod(jReqMi, jReqArguments)
        Expr.CallUnchecked(jReqTyped, [fieldName; codec])
        
    let callMapSymbol (property: FieldDescriptor) =
        let genericMethod = typedefof<Fleece.Newtonsoft.ConcreteCodec<_,_,_,_>>
        let method = genericMethod.GetMethod "op_LessBangGreater"
        let temp = method
        ()
        
    let callmap (descriptor: OneOfDescriptor) (property: PropertyDescriptor) =

        let unionCase =
            let case =
                descriptor.OneOfType
                |> ProvidedUnion.tryGetUnionCaseByPosition (int property.Position)
            match case with
            | Some case -> case
            | None -> failwithf "A union case was not found for: %s" property.ProvidedProperty.Name 
            
        let mapMi =
            Expr.methoddefof <@ FSharpPlus.Operators.map<int, string, int [], string []> x x @>
//            let genericMethod = typedefof<Fleece.Newtonsoft.ConcreteCodec<_,_,_,_>>
//            let method = genericMethod.GetMethod "op_LessBangGreater"
//            method
        
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
            let v = Var("arg0", propertyType)
            let unionCaseInfo = Utils.mkUnionCaseInfo unionCase
            let lambda = Expr.Lambda(v, Expr.NewUnionCaseUnchecked(unionCaseInfo, [Expr.Var(v)] ))
            lambda

        let parameter2 =
        //(Operators.jreq "First_name" (function First_name x -> Some x | _ -> None) )
        //Call (None, jreq,
        //  [Value ("First_name"),
        //   Lambda (_arg1,
        //           IfThenElse (UnionCaseTest (_arg1, First_name),
        //                       Let (x, PropertyGet (Some (_arg1), First_name, []), NewUnionCase (Some, x)),
        //                       NewUnionCase (None)))])
        
            let arg1 = Var("arg1", unionType)
            let ifThenElse =
                let none, some =
                    let optionType = typedefof<_ option>
                    let optionType = ProvidedTypeBuilder.MakeGenericType(optionType, [propertyType])
                    let cases = Reflection.FSharpType.GetUnionCases(optionType)
                    let a, b  = cases.[0], cases.[1]
                    if a.Name = "None" then a,b else b,a
                 
                let arg1Expr = Expr.Var(arg1)
                
                let caseTest =
                    let unionCaseInfo = Utils.mkUnionCaseInfo unionCase
                    Expr.UnionCaseTest(arg1Expr, unionCaseInfo)
                    
                let thenExpr =
                    let xVar = Var("x", propertyType)
                    Expr.Let(xVar, Expr.PropertyGetUnchecked(arg1Expr, property.ProvidedProperty), Expr.NewUnionCase(some, [Expr.Var(xVar)]))
                    
                let elseExpr =
                    Expr.NewUnionCase(none, [])
                Expr.IfThenElse(caseTest, thenExpr, elseExpr)
                
            let lambda = Expr.Lambda(arg1, ifThenElse) 
            let fieldName = Expr.Value property.ProvidedProperty.Name
            let call = callJReq descriptor property fieldName lambda
            call
            
        let expr = Expr.CallUnchecked(mapMiTyped, [ parameter1; parameter2])
        expr
        
    let calljchoice (descriptor: OneOfDescriptor) elements =
        //Operators.jchoice< list<KeyValuePair<string,JToken>>, unionType, unionType>
        let jchoiceMethodInfo = Expr.methoddefof <@ Newtonsoft.Operators.jchoice<seq<_>,_,_> x @>
        let unionType = descriptor.OneOfType :> Type
        let jchoiceArguments = [typeof<KeyValuePair<string,JToken> list>; unionType; unionType]
        let jchoiceMethodInfoTyped = ProvidedTypeBuilder.MakeGenericMethod(jchoiceMethodInfo, jchoiceArguments)
        
        //ConcreteCodec<KeyValuePair<string,JToken> list, KeyValuePair<string,JToken> list, unionType, unionType>    
        let cc = typedefof<ConcreteCodec<_,_,_,_>>
        let keyPairType = typeof<KeyValuePair<string, JsonValue> list>
        let elementType = ProvidedTypeBuilder.MakeGenericType(cc, [keyPairType; keyPairType; unionType; unionType])
        let choiceElements = Expr.NewArray(elementType, elements)
        Expr.CallUnchecked(jchoiceMethodInfoTyped, [choiceElements])
        
    let createJsonObjCodecFromoneOf (descriptor: OneOfDescriptor) =
        let maps =
            descriptor.Properties
            |> Map.toList
            |> List.map (fun (_i, propertyDescriptor) -> callmap descriptor propertyDescriptor)
            
        let jchoice = calljchoice descriptor maps
        let signatureType =
            //ConcreteCodec<KeyValuePair<string, JToken> list, KeyValuePair<string, JToken> list, unionType, unionType> 
            let untypedConcreteCodec = typedefof<ConcreteCodec<_,_,_,_>>
            let unionType = descriptor.OneOfType :> Type
            let arguments = [typeof<KeyValuePair<string, JToken> list>; typeof<KeyValuePair<string, JToken> list>; unionType; unionType]
            let typedConcreteCodec = ProvidedTypeBuilder.MakeGenericType(untypedConcreteCodec, arguments)
            typedConcreteCodec
            
        let jsonObjCodec = ProvidedProperty("JsonObjCodec", signatureType, getterCode = (fun args -> jchoice), isStatic = true )
        jsonObjCodec
        
                        
    let createRecordJFieldOpts (typeDescriptor: TypeDescriptor) =
        let recordType = typeDescriptor.Type :> Type
        let recordFields = typeDescriptor.Fields
        
        let getOptionType (o: Type) = o.GetGenericArguments().[0]
        
        let createJfieldopt (propertyDescriptor: FieldDescriptor) rest =
            match propertyDescriptor with
            | Property propertyDescriptor -> 
                match propertyDescriptor.Rule with
                | ProtoFieldRule.Optional as rule ->
                    let fieldType = getOptionType propertyDescriptor.ProvidedProperty.PropertyType
                    callJfieldopt typeDescriptor.Type rule propertyDescriptor.ProvidedProperty fieldType rest
                | ProtoFieldRule.Repeated as rule -> //will call expand, so type signature affected
                    
                    let fieldType = propertyDescriptor.ProvidedProperty.PropertyType
                    callJfieldopt typeDescriptor.Type rule propertyDescriptor.ProvidedProperty fieldType rest
                | ProtoFieldRule.Required ->
                    failwith "not supported"
            | OneOf oneOf ->
                let fieldType = getOptionType oneOf.CaseProperty.PropertyType
                callJfieldopt typeDescriptor.Type ProtoFieldRule.Optional oneOf.CaseProperty fieldType rest
            | Map map ->
                let fieldType = getOptionType map.ProvidedProperty.PropertyType
                callJfieldopt typeDescriptor.Type ProtoFieldRule.Optional map.ProvidedProperty fieldType rest

        let fieldTypeWithRest =
            let rec loop (recordFields: FieldDescriptor list) =
                [
                    match recordFields with
                    | [] -> ()
                    | head :: [] ->
                        yield createJfieldopt head recordType
                    | head :: tail ->
                        let rest =
                            tail
                            |> List.map (function
                                         | Property propertyDescriptor when propertyDescriptor.Rule = ProtoFieldRule.Repeated ->
                                             typedefof<Option<_>>.MakeGenericType(propertyDescriptor.ProvidedProperty.PropertyType)
                                         | Property { ProvidedProperty = property }
                                         | OneOf    { CaseProperty     = property }
                                         | Map      { ProvidedProperty = property } ->
                                             property.PropertyType )
                            
                        let restAsType = makeFunctionTypeFromElements (rest @ [recordType])
                        yield createJfieldopt head restAsType
                        yield! loop tail
                ]
            loop recordFields
        fieldTypeWithRest
        
    let createJsonObjCodec (typeDescriptor: TypeDescriptor) =     
        let lambdaRecord = createLambdaRecord typeDescriptor
        let mapping = callwithFields lambdaRecord

        let jFieldOpts = createRecordJFieldOpts typeDescriptor
        
        let allPipedFunctions = [yield lambdaRecord; yield mapping; yield! jFieldOpts]
        let foldedFunctions = allPipedFunctions |> List.reduce callPipeRight                                   

#if DEBUG && ADV
        let ctast, ctpt = Quotations.ToAst(foldedFunctions)
        let code = Fantomas.CodeFormatter.FormatAST(ctpt, "test", None, Fantomas.FormatConfig.FormatConfig.Default)
#endif                       

        let signatureType =
            let def = typedefof<Codec<_,_>>
            def.MakeGenericType [|typeof<IReadOnlyDictionary<string,JsonValue>>; typeDescriptor.Type :> _ |]
            
        let createJsonObjCodec = ProvidedProperty("JsonObjCodec", signatureType, getterCode = (fun args -> foldedFunctions), isStatic = true )
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
        static member JsonObjCodec : ConcreteCodec<KeyValuePair<string, JToken> list, KeyValuePair<string, JToken> list, test_oneof, test_oneof> =
            Operators.jchoice<list<KeyValuePair<string,JToken>>, test_oneof, test_oneof>
                [|
                    FSharpPlus.Operators.map<string,
                                             test_oneof,
                                             ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,string,test_oneof>,
                                             ConcreteCodec<KeyValuePair<string,JToken> list, KeyValuePair<string,JToken> list,test_oneof,test_oneof> >
                                                 First_name (Operators.jreq<test_oneof, string> "First_name" (function First_name x -> Some x | _ -> None))
                    FSharpPlus.Operators.map<int,
                                             test_oneof,
                                             ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,int,test_oneof>,
                                             ConcreteCodec<KeyValuePair<string,JToken> list,KeyValuePair<string,JToken> list,test_oneof,test_oneof>>
                                                 Age (Operators.jreq<test_oneof, int> "age" (function Age x -> Some x | _ -> None))
                    FSharpPlus.Operators.map<string,
                                             test_oneof,
                                             ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,string,test_oneof>,
                                             ConcreteCodec<KeyValuePair<string,JToken> list,KeyValuePair<string,JToken> list,test_oneof,test_oneof>>
                                                 Last_name (Operators.jreq<test_oneof, string> "last_name" (function Last_name x -> Some (x) | _ -> None))
                |]
                
        static member PrintDebug() =
            Expr.propertyof <@ test_oneof.JsonObjCodec @>
            |> function x -> x.GetMethod
            |> Expr.TryGetReflectedDefinition
            |> Option.iter (Expr.quotationsTypePrinter >> ignore)
            
[<CLIMutable>]
type SampleMessage =
    { mutable martId : int option
      mutable test_oneof : test_oneof option }
    static member JsonObjCodec =
        fun m t -> {martId = m; test_oneof = t}
        |> withFields
        |> jfieldOpt "martId"  (fun b -> b.martId)
        |> jfieldOpt "test_oneof" (fun a -> a.test_oneof)

open Fleece.Newtonsoft.Operators      
[<CLIMutable>]
type NewSampleMessage =
    { mutable martId : int option
      mutable test_oneof : string option }
    
    [<ReflectedDefinition>]
    static member JsonObjCodec =                        
        fun m t -> {martId = m; test_oneof = t}
        |> ConcreteCodec.map (jopt "martId"  (fun b -> b.martId))
        |> ConcreteCodec.apply (jopt "test_oneof" (fun a -> a.test_oneof))
        
    static member PrintDebug() =
        let property = Expr.propertyof <@ NewSampleMessage.JsonObjCodec @>
        let getMethod = property.GetMethod
        let reflectedDefinition = Expr.TryGetReflectedDefinition getMethod
        reflectedDefinition |> Option.iter (Expr.quotationsTypePrinter >> ignore)

    
type foo =
    | Foo_int of Foo_int : int
    | Foo_string of Foo_string : string
    | Foo_message of Foo_message : TestAllTypes
    static member JsonObjCodec : ConcreteCodec<KeyValuePair<string,JToken> list,KeyValuePair<string,JToken> list,foo,foo> [] =
        [| FSharpPlus.Operators.map<int, foo, ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,Int32,foo>, ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,foo,foo>>
               (fun (arg0 : Int32) -> (Foo_int(arg0) : foo)) (Fleece.Newtonsoft.Operators.jreq<foo, Int32> 
                                                                                      ("Foo_int") (fun (arg1 : foo) -> 
                                                                                      if match arg1 with
                                                                                         | Foo_int _ -> 
                                                                                             true
                                                                                         | _ -> false
                                                                                      then 
                                                                                          let x : Int32 =
                                                                                              match arg1 with
                                                                                              | Foo_int(Foo_int = x) -> 
                                                                                                  x
                                                                                              | _ -> 
                                                                                                  failwith 
                                                                                                      "Should never hit"
                                                                                          (Some(x) : Option<Int32>)
                                                                                      else (None : Option<Int32>)))
           
           FSharpPlus.Operators.map<string, foo, ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,String,foo>, ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,foo,foo>>
               (fun (arg0 : String) -> (Foo_string(arg0) : foo)) 
               (Fleece.Newtonsoft.Operators.jreq<foo, String> ("Foo_string") (fun (arg1 : foo) -> 
                    if match arg1 with
                       | Foo_string _ -> true
                       | _ -> false
                    then 
                        let x : String =
                            match arg1 with
                            | Foo_string(Foo_string = x) -> x
                            | _ -> failwith "Should never hit"
                        (Some(x) : Option<String>)
                    else (None : Option<String>)))
           
           FSharpPlus.Operators.map<TestAllTypes, foo, ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,TestAllTypes,foo>, ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,foo,foo>> 
               (fun (arg0 : TestAllTypes) -> (Foo_message(arg0) : foo)) 
               (Fleece.Newtonsoft.Operators.jreq<foo, TestAllTypes> ("Foo_message") (fun (arg1 : foo) -> 
                    if match arg1 with
                       | Foo_message _ -> true
                       | _ -> false
                    then 
                        let x : TestAllTypes =
                            match arg1 with
                            | Foo_message(Foo_message = x) -> x
                            | _ -> failwith "Should never hit"
                        (Some(x) : Option<TestAllTypes>)
                    else (None : Option<TestAllTypes>))) |]
        
type unionWithOperators =
    | Foo_int of Foo_int : int
    | Foo_string of Foo_string : string
    | Foo_message of Foo_message : TestAllTypes
    static member JsonObjCodec =
        let map a b = (<!>) a b
        let xx = <@ map Foo_int (jreq<unionWithOperators, Int32> "Foo_int" (function Foo_int     x -> Some x | _ -> None)) @>
        let yy = <@ xx @>
        jchoice
            [
                Foo_int     <!> jreq "Foo_int"     (function Foo_int     x -> Some x | _ -> None)
                Foo_string  <!> jreq "Foo_string"  (function Foo_string  x -> Some x | _ -> None)
                Foo_message <!> jreq "Foo_message" (function Foo_message x -> Some x | _ -> None)
            ]
#endif
