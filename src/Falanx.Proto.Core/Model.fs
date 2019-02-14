namespace Falanx.Proto.Core
module Model =
    open Froto.Parser.Ast
    open System
    open ProviderImplementation.ProvidedTypes
    open Froto.Parser.ClassModel
    open Falanx.Machinery
    open Falanx.Machinery.Prelude
    
    type ProtobufType = string
    
    let ProtoTypes =
        Set.ofList [
            "double"
            "float"
            "int32"
            "int64"
            "uint32"
            "uint64"
            "sint32"
            "sint64"
            "fixed32"
            "fixed64"
            "sfixed32"
            "sfixed64"
            "bool"
            "string"
            "bytes" ]
    

    type TypeKind = 
        | Primitive of string
        | Class of scope: string * message: ProtoMessage
        | Enum of scope : string * fullName : string
        | OneOf of scope : string * name : string * fields : POneOfStatement list
        member x.FullName =
            match x with
            | Primitive type' -> type'
            | Class(scope, message) -> scope +.+ message.Name
            | Enum(scope, fullName) -> scope +.+ fullName
            | OneOf(scope, name, fields) -> scope +.+ name
    
    type TypeContext = 
        { Kind: TypeKind
          RuntimeType: Type
          ProtobufType: ProtobufType }
    
        member this.UnderlyingType =
            if this.RuntimeType.IsGenericType
            then this.RuntimeType.GenericTypeArguments.[0]
            else this.RuntimeType
            
    type PropertyDescriptor = 
        { Position: FieldNum
          Rule: ProtoFieldRule 
          Type: TypeContext
          ProvidedProperty: ProvidedProperty
          ProvidedField: ProvidedField option }
          
    type OneOfDescriptor =
        { Properties:  Map<int, PropertyDescriptor>
          Type: TypeContext
          OneOfType: ProvidedUnion
          OneOfMembers: POneOfStatement list
          CaseProperty: ProvidedProperty
          CaseField: ProvidedField }
          
    type MapDescriptor = 
        { KeyType: TypeContext
          ValueType: TypeContext
          Position: FieldNum
          ProvidedProperty: ProvidedProperty
          ProvidedField: ProvidedField }
        
    type FieldDescriptor =
        | Property of PropertyDescriptor
        | OneOf of OneOfDescriptor
        | Map of MapDescriptor
          
    type TypeDescriptor = 
        { Type: ProvidedTypeDefinition
          Properties: PropertyDescriptor list
          OneOfGroups: OneOfDescriptor list
          Maps: MapDescriptor list }
        member x.Fields : FieldDescriptor list =
            [ yield! x.Properties |> List.map Property
              yield! x.OneOfGroups |> List.map OneOf
              yield! x.Maps |> List.map Map ]
            |> List.sortBy (function
                            | Property p -> int p.Position
                            | OneOf o ->
                                o.Properties
                                |> Seq.map ((|KeyValue|) >> fst)
                                |> Seq.min
                            | Map m -> int m.Position  )