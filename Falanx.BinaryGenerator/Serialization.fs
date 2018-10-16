namespace Falanx.BinaryGenerator
module Serialization =
    open Froto.Parser.Ast
    open System
    open System.Reflection
    open Microsoft.FSharp.Quotations
    open ProviderImplementation.ProvidedTypes.UncheckedQuotations
    open Froto.Parser.ClassModel
    open Froto.Serialization
    open Model
    open Falanx.BinaryCodec
    open Falanx.BinaryCodec.Primitives
    open Extensions
    open ProviderImplementation.ProvidedTypes
    open Falanx.Ast
    open Falanx.Ast.Utils
    
            
    let primitiveWriter = function
        | "double" -> <@@ writeDouble @@>
        | "float" -> <@@ writeFloat @@>
        | "int32" -> <@@ writeInt32 @@>
        | "int64" -> <@@ writeInt64 @@>
        | "uint32" -> <@@ writeUInt32 @@>
        | "uint64" -> <@@ writeUInt64 @@>
        | "sint32" -> <@@ writeSInt32 @@>
        | "sint64" -> <@@ writeSInt64 @@>
        | "fixed32" -> <@@ writeFixed32 @@>
        | "fixed64" -> <@@ writeFixed64 @@>
        | "sfixed32" -> <@@ writeSFixed32 @@>
        | "sfixed64" -> <@@ writeSFixed64 @@>
        | "bool" -> <@@ writeBool @@>
        | "string" -> <@@ writeString @@>
        | "bytes" -> <@@ writeBytes @@>
        | x -> failwithf "Type %A nor supported" x
    
    let serializeMapExpr buffer this (map: MapDescriptor) =
        let keyWriter = primitiveWriter map.KeyType.ProtobufType
        let keyType = map.ProvidedProperty.PropertyType.GenericTypeArguments.[0]
        let valueType = map.ProvidedProperty.PropertyType.GenericTypeArguments.[1]
        let positionExpr = Expr.Value(map.Position)
        let mapExpr = Expr.PropertyGet(this, map.ProvidedProperty)
        
        match map.ValueType.Kind with
        | Primitive ->
            Expr.callStaticGeneric 
                [keyType; valueType]
                [keyWriter; primitiveWriter map.ValueType.ProtobufType; positionExpr; buffer; mapExpr]
                <@@ writePrimitiveMap x x x x x @@>
        | Class(_scope, _name) -> 
            Expr.callStaticGeneric
                [keyType; valueType]
                [keyWriter; positionExpr; buffer; Expr.box mapExpr]
                <@@ writeMessageMap x x x x @@>
        | Union _ -> failwith "oneOf map type are not currently supported"
        | Enum(_scope, _name) ->
            Expr.callStaticGeneric
                [keyType]
                [keyWriter; positionExpr; buffer; mapExpr]
                <@@ writeEnumMap x x x x @@>
    
    let callPrimitive writer (prop: PropertyDescriptor) rule position buffer value =
        let args = [position; buffer; value]
        match rule with
        | Required ->
            Expr.apply writer args
        | Optional ->            
            Expr.callStaticGeneric 
                            [prop.Type.UnderlyingType]
                            [writer; position; buffer; value]
                            <@@ writeOption x x x x @@>
        | Repeated -> 
            Expr.callStaticGeneric 
                [prop.Type.UnderlyingType]
                (writer::args)
                <@@ writeRepeated x x x x @@>
                            
    
    let serializeProperty buffer this (prop: PropertyDescriptor) =
        let value = Expr.PropertyGetUnchecked(this, prop.ProvidedProperty)
        let position = Expr.Value(prop.Position)
        
        try
            match prop.Type.Kind, prop.Rule with
            | Class(_scope, _name), Optional -> 
                Expr.callStaticGeneric 
                    [prop.Type.UnderlyingType] 
                    [position; buffer; Expr.box value]  
                    <@@ writeOptionalEmbedded x x x @@>
            | Class(_scope, _name), Repeated ->
                Expr.callStaticGeneric
                    [prop.Type.UnderlyingType]
                    [position; buffer; value]
                    <@@ writeRepeatedEmbedded(x, x, x) @@>
            //required is proto2 specific
            | Class(_scope, _name), Required ->
                <@@ writeEmbedded x x x @@>
                |> Expr.methodof
                |> Expr.callStatic [position; buffer; value]
            | Union _, _ -> failwith "union fields should not be serialized here"
            | Enum(_scope, _name), rule ->
                let intMethod = 
                    let arg = Var("x",prop.Type.UnderlyingType)
                    Expr.Lambda(arg, Expr.callStaticGeneric [prop.Type.UnderlyingType] [Expr.Var arg] <@int x@>)
                let optMap = Expr.callStaticGeneric [prop.Type.UnderlyingType; typeof<int>] [intMethod; value] <@Option.map x x@>
                match rule with 
                | Optional -> 
                    Expr.callStaticGeneric 
                        [typeof<int>]
                        [ <@@ writeInt32 @@> ;position; buffer; optMap]
                        <@@ writeOption x x x x @@>
                | Required -> Expr.apply <@@ writeInt32 @@> [position; buffer; optMap]
                | Repeated -> 
                    Expr.callStaticGeneric 
                        [typeof<int>]
                        [ <@@ writeInt32 @@> ;position; buffer; optMap]
                        <@@ writeRepeated x x x x @@>
            | Primitive, rule ->
                callPrimitive (primitiveWriter prop.Type.ProtobufType) prop rule position buffer value
        with
        | ex -> 
            printfn "Failed to serialize property %s: %O. Error: %O" prop.ProvidedProperty.Name value.Type ex
            reraise()
             
    let serializeUnion buffer this (prop : OneOfDescriptor) =
        match prop.Type.Kind with 
        | Union(_scope, _name, fields) ->
            
            let names (number:int) : string =
                match prop.OneOfType |> ProvidedTypesExtension.ProvidedUnion.tryGetUnionCaseByTag number with
                | Some unionCase -> unionCase.name
                | _ -> ""
            let oneOfExpr = Expr.PropertyGet(this, prop.CaseProperty)
            
            let unioncasesTests =
                (prop.OneOfType.UnionCases, prop.Properties)
                ||> Seq.map2 
                    (fun puc (KeyValue(_k, v)) -> 
                        let case = mkUnionCaseInfo puc.declaringType puc.tag names
                        let optionValue = Expr.callStaticGeneric [puc.declaringType] [oneOfExpr] <@@ Option.get x @@>
                        let testExpr = Quotations.Expr.UnionCaseTest(optionValue, case)
                        let item = ProvidedProperty(puc.name, v.Type.UnderlyingType)
                        item.PatchDeclaringType puc.unionCaseType
                        let descriptor = { v with ProvidedProperty = item; Rule = Required }
                                               
                        let writerExpr = serializeProperty buffer optionValue descriptor
                        testExpr, writerExpr)
                |> Seq.fold(fun acc (guard, writer) -> Expr.IfThenElseUnchecked(guard, writer, acc)) (Expr.Value(()))
                
            
            let someUnionCase =
                let closedOption = ProvidedTypeBuilder.MakeGenericType(typedefof<option<_>>, [prop.Type.UnderlyingType])
                let closedCases = FSharp.Reflection.FSharpType.GetUnionCases(closedOption)
                closedCases |> Array.find(fun uc -> uc.Name = "Some")
            
            let someUnionCaseTest = Quotations.Expr.UnionCaseTest(oneOfExpr, someUnionCase)
            Quotations.Expr.IfThenElseUnchecked(someUnionCaseTest, unioncasesTests, <@@ () @@>)             
        | _ -> failwith "only oneOf types are serializable here"
                                
    
    let createSerializeExpt (typeInfo: TypeDescriptor) this bufferExpr=
        
        let properties =
            if typeInfo.Properties |> List.isEmpty then None else 
            typeInfo.Properties
            |> List.sortBy (fun prop -> prop.Position)
            |> List.map (serializeProperty bufferExpr this)
            |> Some
            
        let oneOfs =
            if typeInfo.OneOfGroups |> List.isEmpty then None else
            typeInfo.OneOfGroups
            |> List.map (fun prop -> serializeUnion bufferExpr this prop)
            |> Some
            
        let maps =
            if typeInfo.Maps |> List.isEmpty then None else
            typeInfo.Maps
            |> List.sortBy (fun map -> map.Position)
            |> List.map (serializeMapExpr bufferExpr this)
            |> Some

        [properties; oneOfs; maps]
        |> List.choose (function Some exprs -> Some (Expr.sequence exprs) | None -> None)
        |> Expr.sequence