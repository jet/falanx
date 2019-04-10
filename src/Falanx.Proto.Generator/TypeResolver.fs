namespace Falanx.Proto.Generator

open System
open Falanx.Proto.Codec.Binary
open Falanx.Proto.Core.Model
open ProviderImplementation.ProvidedTypes
open Falanx.Machinery
open Froto.Parser.ClassModel
open Froto.Parser.Ast
open Falanx.Machinery.Prelude

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
    let discoverTypes (scope: string) (file: ProtoFile): TypesLookup =
    
        let rec processMessage scope (message: ProtoMessage) = seq {
            let fullName = scope +.+ message.Name
            
            //message unions
            yield! message.Parts
                   |> Seq.choose (function
                                  | TOneOf(name, members) ->
                                      Some(TypeKind.OneOf(fullName, name, members))
                                  | _ -> None)

            //messages
            yield TypeKind.Class(scope, message)
            
            //message enums
            yield! message.Enums |> Seq.map (fun enum -> TypeKind.Enum(fullName, enum.Name))
              
            //nested messages     
            yield! message.Messages |> Seq.collect (processMessage fullName)
        }
        
        let rec processFile (file: ProtoFile) (scope: string) =
            seq {
                //imports
                let importsAndScope = 
                        file.Imports
                        |> List.map (fun import -> let protoFile = ProtoFile.fromFile import
                                                   let rootScope = protoFile.Packages |> Seq.tryHead |> Option.defaultValue String.Empty
                                                   rootScope, protoFile)
                for (scope, file) in importsAndScope do
                    yield! processFile file scope
                
                //global enums
                let enums =
                    file.Enums
                    |> Seq.map (fun protoEmum -> scope +.+ protoEmum.Name, (TypeKind.Enum(scope, protoEmum.Name), ProvidedTypeDefinition.mkEnum protoEmum.Name))
                
                yield!
                    file.Messages
                    |> Seq.collect (processMessage scope)
                    |> Seq.map (fun kind -> 
                        let ty : ProvidedTypeDefinition = 
                            match kind with
                            | TypeKind.OneOf(_scope, name, _unionFields) -> ProvidedUnion(name, Some typeof<obj>, isErased = false) :> _
                            | Class(_scope, message) -> ProvidedRecord(message.Name, Some typeof<obj>, isErased = false) :> _
                            | Enum(_scope, name) -> ProvidedTypeDefinition.mkEnum name
                            | Primitive type' -> invalidOpf "Primitive type '%s' does not require custom Type" type'
                        kind.FullName, (kind, ty))
                    |> Seq.append enums
            }   
        processFile file scope
        |> Map.ofSeq
        
    let tryResolveScalar = function
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
        
    let tryResolveNonScalar scope targetType (lookup: TypesLookup) =
        allScopes scope
        |> Seq.map (fun s -> Map.tryFind (s +.+ targetType) lookup )
        |> Seq.tryFind Option.isSome
        |> Option.flatten
    
    let tryResolve scope targetType (lookup: TypesLookup) = 
        let findInLookup () = 
            tryResolveNonScalar scope targetType lookup
            |> Option.map (fun (kind, ty) -> kind, ty :> Type)
    
        tryResolveScalar targetType 
        |> Option.map (fun t -> Primitive targetType, t)
        |> Option.orElseWith findInLookup
        
    let tryResolvePType scope targetType (lookup: TypesLookup) = 
        tryResolve scope (ptypeToString targetType) lookup