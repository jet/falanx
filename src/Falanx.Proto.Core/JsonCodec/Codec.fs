namespace Falanx.Proto.Core
open System
open System.Reflection
open System.Collections.Generic
open System.Diagnostics
open Fleece
open Fleece.Newtonsoft
open Fleece.Newtonsoft.Operators
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

[<DebuggerDisplay "{ToDebuggerDisplay(),nq}">]
type TypeWrapper =
    | Single of Type
    | Function of Type list
    static member create(t:Type) =
        match t with
        | t when FSharpTypeSafe.IsFunction t ->
            Function (Utils.getFunctionElements t)
        | _ -> Single (t)
        
    override this.ToString() =
        match this with
        | Single t -> t.ToString()
        | Function elements ->
            elements
            |> List.map (fun t -> t.ToString())
            |> String.concat " -> "
    
    member this.ToDebuggerDisplay () = this.ToString()

[<AutoOpen>]
module FleeceExtensions =

    type ConcreteCodec<'S1, 'S2, 't1, 't2> with
        static member inline map  (field: ConcreteCodec<'S, 'S, 'f, 'T>) (f) =
            f <!> field
        
        static member inline apply  (currentField: ConcreteCodec<'S, 'S, 'f, 'T>) (remainderFields: ConcreteCodec<'S, 'S, 'f ->'r, 'T>) =
            remainderFields <*> currentField
            
        static member inline fillHole(masterGeneric: ^a) =
             typeof<ConcreteCodec<KeyValuePair<string, JsonValue> list,KeyValuePair<string, JsonValue> list, 'a, 'a>>
            
#nowarn "686"   
module JsonCodec =              
    let inline (|NameAndType|) arg =
        let name = ( ^a : (member Name : string) arg)
        let typ = ( ^a : (member PropertyType : Type) arg)
        name,typ
    
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
                        | OneOf{CaseProperty = NameAndType(n,t)} ->
                            Var(n, t)
                        | Map{ProvidedProperty = property} ->
                            Var(property.Name, property.PropertyType))

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
            List.foldBack (fun var acc -> Expr.Lambda(var, acc)) lambdaVars state
        result
              
    let getFunctionReturnType (typ: Type) =
        let rec loop (typ: Type) = 
            if FSharpTypeSafe.IsFunction typ then
                let _domain, range = FSharpTypeSafe.GetFunctionElements typ                   
                loop range
            else typ  
        let result = loop typ
        result
        
        
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
                
    let callmap (descriptor: OneOfDescriptor) (property: PropertyDescriptor) =

        let unionCase =
            let case =
                descriptor.OneOfType
                |> ProvidedUnion.tryGetUnionCaseByPosition (int property.Position)
            match case with
            | Some case -> case
            | None -> failwithf "A union case was not found for: %s" property.ProvidedProperty.Name 
            
        let mapMi = Expr.methoddefof <@ FSharpPlus.Operators.map<int, string, int [], string []> x x @>
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
                        
    let calljopt (recordType: Type) (property: ProvidedProperty) (rule: ProtoFieldRule) (fieldType: Type ) =
        
        let xvar = Var("x", recordType)
        let propertyExpr = Expr.PropertyGet(Expr.Var xvar, property)
        
        let getter =
            match rule with
            | ProtoFieldRule.Optional ->
                Expr.Lambda(xvar, propertyExpr) //: 'a -> 'b option @>
                
            | ProtoFieldRule.Repeated ->
                let exp = Expr.callStaticGeneric [yield! propertyExpr.Type.GenericTypeArguments] [propertyExpr] <@ expand x @>
                Expr.Lambda(xvar, exp) //: 'a -> 'b option @>
                
            | _ -> failwith "not supported"
          
        let expr = TypeBinder.create jopt<_,string> [recordType; fieldType] [Expr.Value property.Name; getter]
        expr

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
     
    let getOptionType (o: Type) = o.GetGenericArguments().[0]   
        
    let createJopt (recordType: Type) (propertyDescriptor: FieldDescriptor) =
        match propertyDescriptor with
        | Property propertyDescriptor -> 
            match propertyDescriptor.Rule with
            | ProtoFieldRule.Optional as rule ->
                let fieldType = getOptionType propertyDescriptor.ProvidedProperty.PropertyType
                calljopt recordType propertyDescriptor.ProvidedProperty propertyDescriptor.Rule fieldType
            | ProtoFieldRule.Repeated as rule ->
                let fieldType = propertyDescriptor.ProvidedProperty.PropertyType
                calljopt recordType propertyDescriptor.ProvidedProperty propertyDescriptor.Rule fieldType
            | ProtoFieldRule.Required ->
                failwith "not supported"
        | OneOf oneOf ->
            let fieldType = getOptionType oneOf.CaseProperty.PropertyType
            calljopt recordType oneOf.CaseProperty ProtoFieldRule.Optional fieldType
        | Map map ->
            let fieldType = getOptionType map.ProvidedProperty.PropertyType
            calljopt recordType map.ProvidedProperty ProtoFieldRule.Optional fieldType
                        
    let createRecordJFieldOpts (typeDescriptor: TypeDescriptor) =
        let recordType = typeDescriptor.Type :> Type
        let recordFields = typeDescriptor.Fields        
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
        
    let createRecordJopts (typeDescriptor: TypeDescriptor) =
        let recordType = typeDescriptor.Type :> Type
        let recordFields = typeDescriptor.Fields        
        
        let joptwithNext =
            let rec loop (recordFields: FieldDescriptor list) =
                [
                    match recordFields with
                    | [] -> ()
                    | head :: [] ->
                        yield createJopt recordType head//, None
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
                            
                        //let restAsType = makeFunctionTypeFromElements (rest @ [recordType])
                        yield createJopt recordType head//, Some restAsType
                        yield! loop tail
                ]
            loop recordFields
        joptwithNext
    
//    module M =
//        let inline (<*>) (x: ^T) (y: ^U) = ((^T or ^U) : (static member (<*>): ^T * ^U -> ^V) (x, y))
//        let inline (<!>) (x: ^T) (y: ^U) = ((^T or ^U) : (static member (<!>): ^T * ^U -> ^V) (x, y))
    
    //open M

    [<CLIMutable>]
    type NewSampleMessage =
        { mutable myInt : int option
          mutable myString : string option
          mutable myFloat: float option }
        
        [<ReflectedDefinition>]
        static member JsonObjCodec =
            fun i s f -> { myInt = i
                           myString = s
                           myFloat = f }
            <!> jopt<NewSampleMessage,int> "myInt" (fun b -> b.myInt)
            <*> jopt<NewSampleMessage,string> "myString" (fun a -> a.myString)
            <*> jopt<NewSampleMessage,float> "myFloat" (fun a -> a.myFloat)
            
        static member GetQuotedJsonObjCodec() =
            let property = Expr.propertyof <@ NewSampleMessage.JsonObjCodec @>
            let getMethod = property.GetMethod
            let reflectedDefinition = Expr.TryGetReflectedDefinition getMethod
            reflectedDefinition |> Option.iter (Expr.quotationsTypePrinter >> ignore)
            reflectedDefinition.Value
        
    let createJsonObjCodec (typeDescriptor: TypeDescriptor) =     
        let lambdaRecord = createLambdaRecord typeDescriptor
        let mapping = callwithFields lambdaRecord
        let jFieldOpts = createRecordJFieldOpts typeDescriptor
        let allPipedFunctions = [yield lambdaRecord; yield mapping; yield! jFieldOpts]
        let foldedFunctions = allPipedFunctions |> List.reduce callPipeRight                                   

#if DEBUG
        let jsonObjCodec = NewSampleMessage.GetQuotedJsonObjCodec()
        let _ctast, ctpt = Quotations.ToAst(jsonObjCodec)
        let pt = ASTCleaner.untypeParseTree ctpt
        let code = Fantomas.CodeFormatter.FormatAST(pt, "test", None, Fantomas.FormatConfig.FormatConfig.Default)
        let _ = code
#endif                       

        let signatureType =
            let def = typedefof<Codec<_,_>>
            def.MakeGenericType [|typeof<IReadOnlyDictionary<string,JsonValue>>; typeDescriptor.Type :> _ |]
            
        let createJsonObjCodec = ProvidedProperty("JsonObjCodec", signatureType, getterCode = (fun args -> foldedFunctions), isStatic = true )
        createJsonObjCodec
    
    let callMapOperator types args =
        // f: 'T -> 'U  -> x: ^Functor<'T> -> ^Functor<'U> (requires static member Map )
        // 'T is int option //current field
        // 'U is ResizeArray<string> option -> TestAllTypes //nextfield -> nextfield/record
        // 'Functor<'T> is ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,int option,TestAllTypes>
        // 'Functor<'U> is ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,(ResizeArray<string> option -> TestAllTypes),TestAllTypes>
        
        // 0: FSharpOption[int]
        // 1: FSharpFunc[FSharpOption[String], FSharpFunc[FSharpOption[Double], NewSampleMessage]]
        // 2: ConcreteCodec[FSharpList[KeyValuePair[String, JToken]], FSharpList[KeyValuePair[String, JToken]],
        //     FSharpOption[int], NewSampleMessage]
        // 3: ConcreteCodec[FSharpList[KeyValuePair[String, JToken]], FSharpList[KeyValuePair[String, JToken]],
        //     FSharpFunc[FSharpOption[String], FSharpFunc[FSharpOption[Double], NewSampleMessage]], NewSampleMessage]
        
        // 0: int option
        // 1: string option -> float option -> NewSampleMessage
        // 2: ConcreteCodec<KeyValuePair<String, JToken> list, KeyValuePair<String, JToken> list, int option, NewSampleMessage>
        // 3: ConcreteCodec<KeyValuePair<String, JToken> list, KeyValuePair<String, JToken> list, string option -> float option -> NewSampleMessage, NewSampleMessage>
        
        let map =
            TypeBinder.create (fun (a: int option -> string option -> string) (b: ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,int option, string>) -> a <!> b)
                              types 
                              args
        map
               
    let callApplicative types args =
        //static member inline (<*>) (remainderFields: ConcreteCodec<'S, 'S, 'f ->'r, 'T>, currentField: ConcreteCodec<'S, 'S, 'f, 'T>) =
        
        //remainderFields
        //ConcreteCodec<
        // S       KeyValuePair<string,JsonValue> list,
        // S       KeyValuePair<string,JsonValue> list,
        // f -> r (float option -> ResizeArray<string> option -> TestAllTypes),
        // T       TestAllTypes>
   
        //currentField
        //ConcreteCodec<
        // S   KeyValuePair<string,JsonValue> list,
        // S   KeyValuePair<string,JsonValue> list,
        // f   float option,
        // T   TestAllTypes>
        
        //return
        //ConcreteCodec<
        //    KeyValuePair<string,JsonValue> list,
        //    KeyValuePair<string,JsonValue> list,
        //    (ResizeArray<string> option -> TestAllTypes),
        //    TestAllTypes>
        
        
        //remainderFields
        //ConcreteCodec<
        // S      KeyValuePair<string,JsonValue> list,
        // S      KeyValuePair<string,JsonValue> list,
        // f -> r (ResizeArray<string> option -> TestAllTypes),
        // T      TestAllTypes>
        
        //currentField
        //ConcreteCodec<
        // S   KeyValuePair<string,JsonValue> list,
        // S   KeyValuePair<string,JsonValue> list,
        //    ResizeArray<string> option,
        //    TestAllTypes>
        
        //return
        //ConcreteCodec<
        // S   KeyValuePair<string,JsonValue> list,
        // S   KeyValuePair<string,JsonValue> list,
        //    TestAllTypes,
        //    TestAllTypes>
        let app =
            TypeBinder.create (fun (a: ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,(string option -> string),string>) b -> a <*> b)
                              types 
                              args
        app
        
    let mkFunTy a b =
        let funTyC = typeof<(obj -> obj)>.GetGenericTypeDefinition()
        let voidTy = typeof<System.Void>
        let unitTy = typeof<unit>
        let removeVoid a = if a = voidTy then unitTy else a
            
        let (a, b) = removeVoid a, removeVoid b
        if Expr.isGenerated a || Expr.isGenerated b then
            ProvidedTypeBuilder.MakeGenericType(funTyC, [a;b])
        else
            funTyC.MakeGenericType([| a;b |])        
                    
    let callMapAndApply (e1:Expr) (e2: Expr) =
        match e1 with
        
        | Quotations.DerivedPatterns.Lambdas(vars, Patterns.NewRecord(recordType, args)) as lambda
            when (List.concat vars) = (args |> List.choose onlyVar) ->
            //do map
            // f: 'T -> 'U  -> x: ^Functor<'T> -> ^Functor<'U> (requires static member Map )
            // 'T is int option //current field
            // 'U is ResizeArray<string> option -> TestAllTypes //nextfield -> nextfield/record
            // 'Functor<'T> is ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,int option,TestAllTypes>
            // 'Functor<'U> is ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,(ResizeArray<string> option -> TestAllTypes),TestAllTypes>
            
            // 0: int option
            // 1: string option -> float option -> NewSampleMessage
            // 2: ConcreteCodec<KeyValuePair<String, JToken> list, KeyValuePair<String, JToken> list, int option, NewSampleMessage>
            // 3: ConcreteCodec<KeyValuePair<String, JToken> list, KeyValuePair<String, JToken> list, string option -> float option -> NewSampleMessage, NewSampleMessage>
            
            let vars = List.concat vars
            
            let domain, range =
                match vars with
                | h::t ->
                    let extractedTypes = t |> List.map (fun v -> v.Type)
                    let rest = List.foldBack(fun t state -> mkFunTy t state) extractedTypes recordType
                    h.Type, rest    
                | [] -> failwith "empty vars"

            let lambdaType = lambda.Type
            
            let domain, range = FSharpTypeSafe.GetFunctionElements lambdaType
            let codec1, codec2 =
                let concreteCodec = typedefof<ConcreteCodec<_,_,_,_>>
                let keyPairs = typeof<KeyValuePair<string, JsonValue> list>
                ProvidedTypeBuilder.MakeGenericType(concreteCodec, [keyPairs; keyPairs; domain; recordType]),
                ProvidedTypeBuilder.MakeGenericType(concreteCodec, [keyPairs; keyPairs; range; recordType])

            let map = callMapOperator [domain; range; codec1; codec2] [e1; e2]
            map
        | Quotations.Patterns.Call(_instance, methodInfo, arguments) ->
            //do apply
            // map would remove the int option part
            // int option -> string option -> float option -> record
            // apply should be: string option-> float option -> record
               
            //0: ConcreteCodec[KeyValuePair[String,JToken] list, KeyValuePair[String,JToken] list, string option -> float option -> NewSampleMessage, NewSampleMessage]
            //1: ConcreteCodec[KeyValuePair[String,JToken] list, KeyValuePair[String,JToken] list, string option, NewSampleMessage]
            //2: ConcreteCodec[KeyValuePair[String,JToken] list, KeyValuePair[String,JToken] list, float option -> NewSampleMessage, NewSampleMessage]
            
            //0: ConcreteCodec[KeyValuePair[String,JToken] list, KeyValuePair[String,JToken] list, float option -> NewSampleMessage, NewSampleMessage]
            //1: ConcreteCodec[KeyValuePair[String,JToken] list, KeyValuePair[String,JToken] list, float option, NewSampleMessage]
            //2: ConcreteCodec[KeyValuePair[String,JToken] list, KeyValuePair[String,JToken] list, NewSampleMessage, NewSampleMessage]
            let e1Type = e1.Type
            let e1GenericArgs =
                let genericArgs = e1Type.GetGenericArguments()
                genericArgs
                |> Array.map (fun a -> if FSharpTypeSafe.IsFunction a
                                       then
                                            let elements = Utils.getFunctionElements a
                                            makeFunctionTypeFromElements elements
                                       else a )
            
            let e2Type = e2.Type
            let e2GenericArgs =
                let genericArgs = e2Type.GetGenericArguments()
                genericArgs
                |> Array.map (fun a -> if FSharpTypeSafe.IsFunction a
                                       then
                                            let elements = Utils.getFunctionElements a
                                            makeFunctionTypeFromElements elements
                                       else a )
                
            let codec1, codec2, codec3 =
                let concreteCodec = typedefof<ConcreteCodec<_,_,_,_>>
                let keyPairs = typeof<KeyValuePair<string, JsonValue> list>
                let c1, c2 =
                    match e1GenericArgs with
                    | [| _ ; _ ; t3; t4|] -> t3, t4
                    | _ -> failwith "unexpected number of arguments"
                    
                let c3, c4 =
                    match e2GenericArgs with
                    | [| _ ; _ ; t3; t4|] -> t3, t4
                    | _ -> failwith "unexpected number of arguments"
                
                //let c1,c2,c3,c4 = TypeWrapper.create c1, TypeWrapper.create c2, TypeWrapper.create c3, TypeWrapper.create c4
                let _domain, range = FSharpTypeSafe.GetFunctionElements c1
                                  
                ProvidedTypeBuilder.MakeGenericType(concreteCodec, [keyPairs; keyPairs; c1; c2 ]),
                ProvidedTypeBuilder.MakeGenericType(concreteCodec, [keyPairs; keyPairs; c3; c4]),
                ProvidedTypeBuilder.MakeGenericType(concreteCodec, [keyPairs; keyPairs; range; c4])
                
            let app = callApplicative [codec1; codec2; codec3] [e1;e2]
            app
        | exp -> failwithf "unknown expression: %A" exp
        
        
    let createJsonObjCodecConcrete (typeDescriptor: TypeDescriptor) =     
        let lambdaRecord = createLambdaRecord typeDescriptor
        
        let signatureType = ConcreteCodec.fillHole typeDescriptor.Type
        
        let jFieldOpts = createRecordJopts typeDescriptor

        let functions = [yield lambdaRecord; yield! jFieldOpts]
        
        let foldedFunctions =
            functions
            |> List.reduce callMapAndApply
        
        let createJsonObjCodec = ProvidedProperty("JsonObjCodecConcrete", signatureType, getterCode = (fun args -> foldedFunctions), isStatic = true )
        createJsonObjCodec
 
#if DEBUG
#nowarn "686"
[<CLIMutable>]
type TestAllTypes =
    { mutable singleInt32 : int option
      mutable singlefloat : float option
      mutable repeatedString : string ResizeArray }
    
    [<ReflectedDefinition>]
    static member JsonObjCodec =
        fun singleInt32 singlefloat repeatedString -> {singleInt32 = singleInt32; singlefloat = singlefloat; repeatedString = flatten<string> repeatedString}
        <!> jopt "singleInt32"  (fun x -> x.singleInt32)
        <*> jopt "singlefloat"  (fun x -> x.singlefloat)
        <*> jopt "repeatedString"  (fun x -> expand<string> x.repeatedString)
        
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
    
type foo =
    | Foo_int of Foo_int : int
    | Foo_string of Foo_string : string
    | Foo_message of Foo_message : TestAllTypes
    static member JsonObjCodec : ConcreteCodec<KeyValuePair<string,JToken> list,KeyValuePair<string,JToken> list,foo,foo> [] =
        [| FSharpPlus.Operators.map<int, foo, ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,Int32,foo>, ConcreteCodec<KeyValuePair<string,JsonValue> list,KeyValuePair<string,JsonValue> list,foo,foo>>
               (fun (arg0 : Int32) -> (Foo_int(arg0) : foo)) (Fleece.Newtonsoft.Operators.jreq<foo, Int32> 
                                                                                      ("Foo_int") (fun (arg1 : foo) -> 
                                                                                      if match arg1 with
                                                                                         | Foo_int _ -> true
                                                                                         | _ -> false
                                                                                      then 
                                                                                          let x : Int32 =
                                                                                              match arg1 with
                                                                                              | Foo_int(Foo_int = x) -> x
                                                                                              | _ -> failwith "Should never hit"
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
