namespace Falanx.BinaryGenerator

open System
open Falanx.BinaryCodec
open Model
open ProviderImplementation.ProvidedTypes
open ProvidedTypes
open Froto.Parser.ClassModel
open Froto.Parser.Ast
open Falanx.Ast.Prelude
open Falanx.Ast.ProvidedTypesExtension

type internal TypesLookup = Map<string, TypeKind * ProvidedTypeDefinition>

module TypeResolver =
        
    let private getShortName (fullName: string) = fullName.Split('.') |> Seq.last
    
    let rec private allScopes (scope: string) = seq{
        yield scope
        if scope <> String.Empty then
            let nextScope = 
                match scope.LastIndexOf "." with
                | index when index > 0 -> scope.Substring(0, index)
                | _ -> String.Empty
            yield! allScopes nextScope
    } 
    
    ///discover/create all top level type skeletons
    let discoverTypes scope (file: ProtoFile): TypesLookup =
    
        let rec loop scope (message: ProtoMessage) = seq {
            let fullName = scope +.+ message.Name
            
            //message unions
            yield! message.Parts
                   |> Seq.choose (function
                                  | TOneOf(name, members) ->
                                      Some(TypeKind.Union(fullName, name, members), fullName +.+ name)
                                  | _ -> None)

            //messages
            yield Class(scope, message.Name), fullName
            
            //message enums
            yield! message.Enums |> Seq.map (fun enum -> TypeKind.Enum(fullName, enum.Name), fullName +.+ enum.Name)
              
            //nested messages     
            yield! message.Messages |> Seq.collect (loop fullName)
        }
        
        //global enums
        let enums =
            file.Enums
            |> Seq.map (fun protoEmum -> scope +.+ protoEmum.Name, (TypeKind.Enum(scope, protoEmum.Name), ProvidedTypes.enum protoEmum.Name))
        
        file.Messages
        |> Seq.collect (loop scope)
        |> Seq.map (fun (kind, fullName) -> 
            let ty = 
                match kind with
                | Union(_scope, name, _unionFields) -> ProvidedUnion(name, Some typeof<obj>, isErased = false) :> ProvidedTypeDefinition
                | Class(scope, name) -> ProvidedRecord(name, Some typeof<obj>, isErased = false) :> _
                | Enum(scope, name) -> ProvidedTypes.enum name
                | Primitive -> invalidOpf "Primitive type '%s' does not require custom Type" fullName
            fullName, (kind, ty))
        |> Seq.append enums
        |> Map.ofSeq
        
    let resolveScalar = function
        | "double" -> Some typeof<proto_double>
        | "float" -> Some typeof<proto_float>
        | "int32" -> Some typeof<proto_int32>
        | "int64" -> Some typeof<proto_int64>
        | "uint32" -> Some typeof<proto_uint32>
        | "uint64" -> Some typeof<proto_uint64>
        | "sint32" -> Some typeof<proto_sint32>
        | "sint64" -> Some typeof<proto_sint64>
        | "fixed32" -> Some typeof<proto_fixed32>
        | "fixed64" -> Some typeof<proto_fixed64>
        | "sfixed32" -> Some typeof<proto_sfixed32>
        | "sfixed64" -> Some typeof<proto_sfixed64>
        | "bool" -> Some typeof<proto_bool>
        | "string" -> Some typeof<proto_string>
        | "bytes" -> Some typeof<proto_bytes>
        | _ -> None
        
    let ptypeToString = function
        | TDouble -> "double"
        | TFloat -> "float"
        | TInt32 -> "int32" 
        | TInt64 -> "int64"
        | TUInt32 -> "uint32"
        | TUInt64 -> "uint64"
        | TSInt32 -> "sint32"
        | TSInt64 -> "sint64"
        | TFixed32 -> "fixed32"
        | TFixed64 -> "fixed64"
        | TSFixed32 -> "sfixed32"
        | TSFixed64 -> "sfixed64"
        | TBool -> "bool"
        | TString -> "string"
        | TBytes -> "bytes"
        | TIdent typeIdent -> typeIdent 
        
    let resolveNonScalar scope targetType (lookup: TypesLookup) =
        allScopes scope
        |> Seq.map (fun s -> Map.tryFind (s +.+ targetType) lookup )
        |> Seq.tryFind Option.isSome
        |> Option.flatten
    
    let resolve scope targetType (lookup: TypesLookup) = 
        let findInLookup () = 
            resolveNonScalar scope targetType lookup
            |> Option.map (fun (kind, ty) -> kind, ty :> Type)
    
        resolveScalar targetType 
        |> Option.map (fun t -> Primitive, t)
        |> Option.orElseWith findInLookup
        
    let resolvePType scope targetType (lookup: TypesLookup) = 
        resolve scope (ptypeToString targetType) lookup
    
