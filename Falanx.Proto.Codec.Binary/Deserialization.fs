namespace Falanx.Proto.Codec.Binary
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open System
open Froto.Parser.ClassModel
open Froto.Serialization.Encoding
open Falanx.Proto.Core.Model
open Falanx.Machinery
open Falanx.Machinery.Expr
open Utils
open Froto.Serialization
open Falanx.Proto.Codec.Binary.Primitives
                                 
/// Contains an implementation of deserialization methods for types generated from ProtoBuf messages
module Deserialization =
    
    let private primitiveReader typ =
        match typ with
        | "double" -> <@@ readDouble @@>
        | "float" -> <@@ readFloat @@>
        | "int32" -> <@@ readInt32 @@>
        | "int64" -> <@@ readInt64 @@>
        | "uint32" -> <@@ readUInt32 @@>
        | "uint64" -> <@@ readUInt64 @@>
        | "sint32" -> <@@ readSInt32 @@>
        | "sint64" -> <@@ readSInt64 @@>
        | "fixed32" -> <@@ readFixed32 @@>
        | "fixed64" -> <@@ readFixed64 @@>
        | "sfixed32" -> <@@ readSFixed32 @@>
        | "sfixed64" -> <@@ readSFixed64 @@>
        | "bool" -> <@@ readBool @@>
        | "string" -> <@@ readString @@>
        | "bytes" -> <@@ readBytes @@>
        | x -> failwithf "Type not supported: %A" x
    
    /// Creates quotation that converts RawField quotation to target property type
    let private deserializeField (property: PropertyDescriptor) (rawField: Expr) =
        match property.Type.Kind with
        | Primitive -> Expr.Application(primitiveReader property.Type.ProtobufType, rawField)
        | Enum(_scope, _name) ->    
            let enumMethodDef = Expr.methoddefof(<@ enum<DayOfWeek> x @>)
            let enumMethod = MethodSymbol2(enumMethodDef,[|property.Type.UnderlyingType|])
            Expr.Call(enumMethod, [ <@@ readInt32 (%%rawField) @@> ])
        | Class(_scope, _name) -> Expr.callStaticGeneric [property.Type.UnderlyingType] [rawField ] <@@ readEmbedded<Template> x @@>
        | Union _ -> failwith "Not implemented"
    
    let private samePosition field idx = <@@ (%%field: RawField).FieldNum = idx @@>
    
    /// Adds key-value pair to the property that corresponds the map 
    let private handleMapElement this mapDescriptor field = 
        let map = Expr.PropertyGet(this, mapDescriptor.ProvidedProperty)
        let keyReader = primitiveReader mapDescriptor.KeyType.ProtobufType
    
        let readMethod, args =
            match mapDescriptor.ValueType.Kind with
            | Primitive -> 
                <@@ readMapElement x x x x @@>,
                [map; keyReader; primitiveReader mapDescriptor.ValueType.ProtobufType; field]
            | Enum(_scope, _name) -> 
                <@@ readMapElement x x x x @@>,
                [map; keyReader; <@@ readInt32 @@>; field]
            | Class(_scope, _name) -> 
                <@@ readMessageMapElement<_, Template> x x x @@>,
                [map; keyReader; field ]
            | Union _ -> failwith "Not implemented"
                    
        Expr.callStaticGeneric
            (map.Type.GenericTypeArguments |> List.ofArray)
            args
            readMethod
                          
    let private handleOptional this propertyDescriptor field =
        let value = deserializeField propertyDescriptor field
        let unionCase = 
            //An alternative method here is to use:
            //FSharpType.GetUnionCases(
            // TypeSymbol(
            //   TypeSymbolKind.OtherGeneric(typedefof<option<_>>),[|propertyDescriptor.Type.UnderlyingType|])) 
            FSharp.Reflection.FSharpType.GetUnionCases(propertyDescriptor.Type.RuntimeType)
            |> Seq.find (fun x -> x.Name = "Some")
        let value = Expr.NewUnionCaseUnchecked(unionCase, [value] )
        Expr.PropertySet(this, propertyDescriptor.ProvidedProperty, value)
        
            
   
        
    let private handleOptionalUnion (this: Expr) (unionProp : ProvidedProperty) (propertyDescriptor: PropertyDescriptor) (providedUnion: ProvidedUnion) (field: Expr) =
        let value = deserializeField propertyDescriptor field
        //create a union constructor and feed the deserialised field into it
        let names (number:int) : string =
            match providedUnion |> ProvidedUnion.tryGetUnionCaseByTag number with
            | Some unionCase -> unionCase.name
            | _ -> ""
        match providedUnion |> ProvidedUnion.tryGetUnionCaseByPosition (int propertyDescriptor.Position) with 
        | Some puc ->
            let unionCaseInfo = mkUnionCaseInfo puc.declaringType puc.tag names
            let unionCtor = Expr.NewUnionCaseUnchecked(unionCaseInfo, [value] )
            let wrappedAsOption = 
                let uinfo = 
                    FSharp.Reflection.FSharpType.GetUnionCases(TypeSymbol(TypeSymbolKind.OtherGeneric(typedefof<option<_>>),[|providedUnion|])) 
                    |> Seq.find (fun x -> x.Name = "Some")
                Expr.NewUnionCase(uinfo,[unionCtor])
            Expr.PropertySetUnchecked(this, unionProp, wrappedAsOption)
        | None -> failwithf "Unable to find union case by position: %i for %s"  propertyDescriptor.Position providedUnion.Name
        
    let private handleRepeated this propertyDescriptor field =
        let value = deserializeField propertyDescriptor field
        let list = Expr.PropertyGet(this, propertyDescriptor.ProvidedProperty)
        let addMethod = list.Type.GetMethod("Add")
        let test = Expr.callStaticGeneric [propertyDescriptor.Type.RuntimeType] [list] <@@ Microsoft.FSharp.Core.Operators.isNull x @@>
        let instantiate = Expr.DefaultValue propertyDescriptor.Type.RuntimeType
        let set = Expr.FieldSet(this, propertyDescriptor.ProvidedField.Value, instantiate)
        Expr.sequence
            [ Expr.IfThenElse( test, set, <@()@> )
              Expr.CallUnchecked(list, addMethod, [value]) ]
    
    let readFrom (typeInfo: TypeDescriptor) (this:Expr) (allFields:Expr) =
    
        try
            // handlers for all properties, depending on position
            let handlers = 
                //create property handlers
                let properties = 
                    typeInfo.Properties
                    |> Seq.map (fun prop -> 
                        match prop.Rule with
                        | Required -> failwith "Not supported in proto 3"
                        | Optional -> prop.Position, handleOptional this prop
                        | Repeated -> prop.Position, handleRepeated this prop)

                //create oneOf handlers
                //SPEC: Setting a oneof field will automatically clear all other members of the oneof. 
                // So if you set several oneof fields, only the last field you set will still have a value
                let oneOfHandlers = 
                    typeInfo.OneOfGroups
                    |> Seq.map (fun descriptor -> descriptor.Properties
                                                  |> Seq.map (fun (KeyValue(_, prop)) -> prop.Position, handleOptionalUnion this descriptor.CaseProperty prop descriptor.OneOfType))
                    |> Seq.concat
                    
                //create map handlers
                let mapHandlers = typeInfo.Maps |> Seq.map(fun map -> map.Position, handleMapElement this map)
                
                [yield! properties
                 yield! oneOfHandlers
                 yield! mapHandlers]
                          
            Expr.forLoop <@@ ZeroCopyBuffer.allFields %%allFields @@> (fun field ->
                handlers
                |> List.fold
                    (fun acc (pos, handler) ->
                        Expr.IfThenElse(
                            samePosition field (int pos),
                            handler field,
                            acc))
                    (Expr.Value(())))
    
        with
        | ex ->
            printfn "Failed to generate Deserialize method for type %s. Details: %O" typeInfo.Type.Name ex
            reraise()
            
    let createReadFromMethod typeInfo = 
        let readFrom = 
            ProvidedMethod(
                "ReadFrom",
                [ProvidedParameter("buffer", typeof<ZeroCopyBuffer>)],
                typeof<Void>,
                invokeCode = (fun args -> readFrom typeInfo args.[0] args.[1]))
        readFrom
        
    let createDeserializeMethod targetType =
        let deserializeMethod =
            let bufferProperty = ProvidedParameter("buffer", typeof<ZeroCopyBuffer>)
            ProvidedMethod(
                "Deserialize", 
                [bufferProperty], 
                targetType.Type,
                invokeCode = (fun args -> Expr.callStaticGeneric [targetType.Type] [args.[0]] <@@ deserialize<Template> x @@>),
                isStatic = true)
        deserializeMethod