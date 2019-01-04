namespace Falanx.Proto.Core
module Model =
    open Froto.Parser.Ast
    open System
    open ProviderImplementation.ProvidedTypes
    open Froto.Parser.ClassModel
    open Falanx.Machinery
    
    type ProtobufType = string

    type TypeKind = 
        | Primitive
        | Class of scope: string * name : string
        | Enum of scope : string * fullName : string
        | Union of scope : string * name : string * fields : POneOfStatement list
    
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
            [
                yield! x.Properties |> List.map Property
                yield! x.OneOfGroups |> List.map OneOf
                yield! x.Maps |> List.map Map
            ]