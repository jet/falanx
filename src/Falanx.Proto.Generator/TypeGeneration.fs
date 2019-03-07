namespace Falanx.Proto.Generator

module TypeGeneration =
    open System
    open System.Reflection
    open Falanx.Proto.Codec.Binary
    open Falanx.Proto.Core
    open Falanx.Proto.Core.Model
    open Froto.Parser.Ast
    open Froto.Parser.ClassModel
    open ProviderImplementation.ProvidedTypes
    open Falanx.Machinery
    open Falanx.Machinery.Prelude
    open Falanx.Machinery.Reflection
    
    type Codec =
        | Binary
        | Json

    let applyRule rule (fieldType: Type) = 
        match rule with
        | Required -> fieldType
        | Repeated -> Expr.makeGenericType [fieldType] typedefof<proto_repeated<_>>
        | Optional -> Expr.makeGenericType [fieldType] typedefof<option<_>>
        
    let private createPropertyDescriptor scope (lookup: TypesLookup) (ty: ProvidedTypeDefinition) getFieldTag (field: ProtoField) =
    
        let typeKind, propertyType = 
            match TypeResolver.resolve scope field.Type lookup with
            | Some(kind, t) -> kind, t
            | None -> failwithf "Message definition not found: %A" field.Type
        
        let propertyType = applyRule field.Rule propertyType
        let propertyName = Naming.snakeToCamel field.Name
    
        let property, backingField = 
            match field.Rule with
            | Repeated ->
                ProvidedTypeDefinition.mkRecordPropertyWithField propertyType propertyName true
            | _ ->
                ProvidedTypeDefinition.mkRecordPropertyWithField propertyType propertyName false
            
        //apply custom attributes for record field
        let constructorInfo = typeof<CompilationMappingAttribute>.TryGetConstructor([|typeof<SourceConstructFlags>; typeof<int> |])
        // a records field has attributes that look like: (where n is field number)"
        // [CompilationMapping(SourceConstructFlags.Field, n)]
        let arguments = 
            [| CustomAttributeTypedArgument (typeof<SourceConstructFlags>, SourceConstructFlags.Field)
               CustomAttributeTypedArgument (typeof<int>, getFieldTag())|]
        property.AddCustomAttribute(CustomAttributeData.Make(constructorInfo.Value, args = arguments))
    
        { ProvidedProperty = property
          ProvidedField = Some backingField
          Position = uint32 field.Position
          Rule = field.Rule
          Type = 
            { ProtobufType = field.Type
              Kind = typeKind
              RuntimeType = propertyType } }
                     
    let createEnum scope lookup (enum: ProtoEnum) =
        let providedEnum = 
            match TypeResolver.resolveNonScalar scope enum.Name lookup with
            | Some(_typeKind, providedType) -> providedType
            | None -> failwithf "Enum '%s' is not defined" enum.Name
        
        enum.Items
        |> Seq.map (fun item -> Naming.upperSnakeToPascal item.Name, item.Value)
        |> ProvidedTypeDefinition.addEnumValues providedEnum
        
        providedEnum 
        
    let private createMapDescriptor scope typesLookup (name: string) (keyTy: PKeyType) (valueTy: PType) position getFieldTag =
        let keyTypeName = 
            let keyType =
                match keyTy with 
                | TKInt32 -> TInt32
                | TKInt64 -> TInt64 
                | TKUInt32 -> TUInt32
                | TKUInt64 -> TUInt64 
                | TKSInt32 -> TSInt32
                | TKSInt64 -> TSInt64
                | TKFixed32 -> TFixed32
                | TKFixed64 -> TFixed64
                | TKSFixed32 -> TSFixed32
                | TKSFixed64 -> TSFixed64
                | TKBool -> TBool
                | TKString -> TString
            TypeResolver.ptypeToString keyType
            
        let valueTypeName = TypeResolver.ptypeToString valueTy
        let valueTypeKind, valueType = 
            match TypeResolver.resolve scope valueTypeName typesLookup with
            | Some (Enum _ as e, _) ->
                e, typeof<proto_int32>
            | Some(kind, ty) ->
                kind, ty
            | None -> failwithf "Can't resolve type '%s'" valueTypeName               
    
        let keyType = 
            match TypeResolver.resolveScalar keyTypeName with
            | Some x -> x
            | None -> failwithf "Can't resolve scalar type '%s'" keyTypeName
        
        let mapType = Expr.makeGenericType [ keyType; valueType] typedefof<proto_map<_, _>>
    
        let property, field = ProvidedTypeDefinition.mkRecordPropertyWithField mapType (Naming.snakeToPascal name) true
        //apply custom attributes for record field
        let constructorInfo = typeof<CompilationMappingAttribute>.TryGetConstructor([|typeof<SourceConstructFlags>; typeof<int> |])
        // a records field has attributes that look like: (where n is field number)"
        // [CompilationMapping(SourceConstructFlags.Field, n)]
        let arguments = 
            [| CustomAttributeTypedArgument (typeof<SourceConstructFlags>, SourceConstructFlags.Field)
               CustomAttributeTypedArgument (typeof<int>, getFieldTag())|]
        property.AddCustomAttribute(CustomAttributeData.Make(constructorInfo.Value, args = arguments))
        
        { KeyType = 
            { ProtobufType = keyTypeName
              RuntimeType = keyType
              Kind = Primitive valueTypeName}
          ValueType = 
            { ProtobufType = valueTypeName
              RuntimeType = valueType
              Kind = valueTypeKind }
          Position = position
          ProvidedProperty = property
          ProvidedField = field }   
                             
    let createOneOfDescriptor scope (typesLookup: TypesLookup) (name: string) (members: POneOfStatement list) getFieldTag =
        let unionKind, unionType =
            match TypeResolver.resolveNonScalar scope name typesLookup with
            | Some (k, u) -> k, u :?> ProvidedUnion
            | None -> failwithf "unknown union type: %s" name
   
        let propertyType = applyRule ProtoFieldRule.Optional unionType
        let propertyName = Naming.snakeToCamel name
        let caseProperty, caseField = ProvidedTypeDefinition.mkRecordPropertyWithField propertyType propertyName false
        //apply custom attributes for record field
        let constructorInfo = typeof<CompilationMappingAttribute>.TryGetConstructor([|typeof<SourceConstructFlags>; typeof<int> |])
        // a records field has attributes that look like: (where n is field number)"
        // [CompilationMapping(SourceConstructFlags.Field, n)]
        let arguments = 
            [| CustomAttributeTypedArgument (typeof<SourceConstructFlags>, SourceConstructFlags.Field)
               CustomAttributeTypedArgument (typeof<int>, getFieldTag())|]
        caseProperty.AddCustomAttribute(CustomAttributeData.Make(constructorInfo.Value, args = arguments))
   
        let propertyDescriptors =
            members
            |> List.mapi (fun i (TOneOfField(name, ptype, position, _)) ->
                let kind, propertyType =
                    match TypeResolver.resolvePType scope ptype typesLookup with
                    | Some (kind, ty) ->
                        let actualType = match kind with TypeKind.Enum _ -> typeof<int32> | _ -> ty
                        kind, actualType
                    | None -> failwithf "Unable to find type %A" ptype
                
                //in this instance we dont need the backing field
                let caseName = Naming.snakeToPascalSnake name
                let unionCaseProperty, _unionCaseField = ProvidedTypeDefinition.mkPropertyWithField propertyType caseName false
                //We snakeToPascal here to keep union cases named correctly for the compiler
                unionType.AddUnionCase(i, int position, caseName, [unionCaseProperty] )
                    
                let propertyInfo =
                    { ProvidedProperty = unionCaseProperty
                      Position = position
                      Rule = Optional
                      ProvidedField = None
                      Type =
                        { ProtobufType = TypeResolver.ptypeToString ptype
                          Kind = kind
                          RuntimeType = propertyType } }
    
                i, propertyInfo)
            
        //Union type internals information
        // [CompilationMapping(SourceConstructFlags.UnionCase, 0)]
        // public static T NewFirst_Name(string item) {
        //     return new First_Name(item); }
        //
        // [CompilationMapping(SourceConstructFlags.UnionCase, 1)]
        // public static T NewLast_Name(string item) {
        //     return new Last_Name(item); }

        //create Tags so FSharp.Reflection thinks this is a real union
        // public static class Tags {
        //     public const int One = 0;
        //     public const int Two = 1; }
        { Properties = propertyDescriptors |> Map.ofList
          Type = { Kind = unionKind; ProtobufType = name; RuntimeType = propertyType }
          OneOfType = unionType
          OneOfMembers = members
          CaseProperty = caseProperty
          CaseField = caseField }        
            
    let rec createType (container: ProvidedTypeDefinition) scope (lookup: TypesLookup) (codecs: Codec Set) (message: ProtoMessage) : ProvidedTypeDefinition = 
         try
             let getFieldTag =
                 let currentTag = ref 0
                 fun() ->
                     let tag = !currentTag
                     incr currentTag
                     tag
             
             let _kind, providedType = 
                 match TypeResolver.resolveNonScalar scope message.Name lookup with
                 | Some x -> x
                 | None -> failwithf "Type '%s' is not defined" message.Name
                  
             let nestedScope = scope +.+ message.Name
             providedType.PatchDeclaringType container
             
             message.Enums
             |> Seq.map (createEnum nestedScope lookup)
             |> Seq.iter providedType.AddMember
             
             message.Messages
             |> Seq.map (createType providedType nestedScope lookup codecs)
             |> Seq.iter providedType.AddMember
             
             let oneOfDescriptors = 
                 message.Parts 
                 |> List.choose (function
                                 | TOneOf(name, members) ->
                                     Some(createOneOfDescriptor nestedScope lookup name members getFieldTag)
                                 | _ -> None)

             if not oneOfDescriptors.IsEmpty then
                 oneOfDescriptors
                 |> List.iter (fun oneOfDescriptor ->
                     providedType.AddMembers [ oneOfDescriptor.OneOfType :> MemberInfo
                                               oneOfDescriptor.CaseField :> _
                                               oneOfDescriptor.CaseProperty :> _ ] 
                     //also add json static property for json codec if present
                     if codecs.Contains Json then
                         let jsonObjCodec = JsonCodec.createJsonObjCodecFromoneOf oneOfDescriptor
                         oneOfDescriptor.OneOfType.AddMember jsonObjCodec
                     )

             let properties =
                 message.Fields
                 |> List.sortBy(fun f-> f.Position)
                 |> List.map (createPropertyDescriptor nestedScope lookup providedType getFieldTag)
        
             for prop in properties do
                 providedType.AddMember prop.ProvidedProperty
                 prop.ProvidedField |> Option.iter providedType.AddMember
        
             let maps = 
                 message.Parts
                 |> List.choose (function
                                | TMap(name, keyTy, valueTy, position, _) ->
                                    Some(createMapDescriptor nestedScope lookup name keyTy valueTy position getFieldTag)
                                | _ -> None)
        
             for map in maps do
                 providedType.AddMember map.ProvidedField
                 providedType.AddMember map.ProvidedProperty
        
             let typeInfo = { Type = providedType; Properties = properties; OneOfGroups = oneOfDescriptors; Maps = maps}
        
             codecs
             |> Set.iter (
                fun codec -> 
                    match codec with
                    | Binary ->
                        let staticSerializeMethod = Falanx.Proto.Codec.Binary.Serialization.createSerializeMethod typeInfo
                        let staticDeserializeMethod = Falanx.Proto.Codec.Binary.Deserialization.createDeserializeMethod typeInfo
                        providedType.AddMember staticSerializeMethod
                        providedType.AddMember staticDeserializeMethod

                        providedType.AddInterfaceImplementation typeof<IMessage>

                        let serializeMethod = Falanx.Proto.Codec.Binary.Serialization.createInstanceSerializeMethod typeInfo staticSerializeMethod
                        providedType.AddMember serializeMethod
                        providedType.DefineMethodOverride(serializeMethod, typeof<IMessage>.GetMethod("Serialize"))

                        let readFromMethod = Falanx.Proto.Codec.Binary.Deserialization.createReadFromMethod typeInfo
                        providedType.AddMember readFromMethod
                        providedType.DefineMethodOverride(readFromMethod, typeof<IMessage>.GetMethod("ReadFrom"))

                        let serializedLengthMethod = Falanx.Proto.Codec.Binary.Serialization.createSerializedLength typeInfo
                        providedType.AddMember serializedLengthMethod
                        providedType.DefineMethodOverride(serializedLengthMethod, typeof<IMessage>.GetMethod("SerializedLength"))
                    | Json ->
#if DEBUG
                        let testCodec = unionWithOperators.JsonObjCodec
                        NewSampleMessage.PrintDebug()
#endif
                        let jsonObjCodec = JsonCodec.createJsonObjCodec typeInfo
                        providedType.AddMember jsonObjCodec
                        )
                          
             providedType
         with
         | ex ->
             printfn "An error occurred while generating type for message %s: %O" message.Name ex
             reraise()         
              
              
    /// For the given package e.g. "foo.bar.baz.abc" creates a hierarchy of nested types Foo.Bar.Baz.Abc 
    /// and returns pair of the first and last types in the hirarchy, Foo and Abc in this case
    let createNamespaceContainer (package: string) =
    
        let rec loop names (current: ProvidedTypeDefinition) =
            match names with
            | [] -> current
            | x::xs -> 
                let nested = ProvidedTypeDefinition(Naming.snakeToPascal x, Some typeof<obj>, isErased = false)
                current.AddMember nested
                loop xs nested
                
        match package.Split('.') |> List.ofArray with
        | rootName::rest ->
            let root = ProvidedTypeDefinition(Naming.snakeToPascal rootName, Some typeof<obj>, isErased = false)
            let deepest = loop rest root
            root, deepest
        | _ -> invalidArg "package" "Package name cannot be empty."

