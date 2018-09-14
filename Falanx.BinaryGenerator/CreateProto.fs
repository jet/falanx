namespace Falanx.BinaryGenerator

type TypeContainer = class end
    
module Proto =
    open Falanx.Ast
    open Falanx.Ast.ProvidedTypesExtension
    open System
    open Microsoft.FSharp.Compiler.Ast
    open Froto.Parser.ClassModel
    open FsAst
    open ProviderImplementation.ProvidedTypes
    open Model
    
    let createProvidedTypes protoDef defaultnamespace =
        let provider = 
            ProvidedTypeDefinition(
                Reflection.Assembly.GetCallingAssembly(),
                defaultnamespace,
                typeof<TypeContainer>.Name,
                Some typeof<obj>, 
                hideObjectMethods = true, 
                isErased = false)
                
        let protoFile = ProtoFile.fromString protoDef
                                    
        let rootScope = protoFile.Packages |> Seq.tryHead |> Option.defaultValue String.Empty
    
        let container = 
            if String.IsNullOrEmpty rootScope
            then provider 
            else
                let root, deepest = TypeGeneration.createNamespaceContainer rootScope
                provider.AddMember root
                deepest
    
        let lookup = TypeResolver.discoverTypes rootScope protoFile
        
        protoFile.Enums
        |> Seq.map (TypeGeneration.createEnum rootScope lookup)
        |> Seq.iter container.AddMember
        
        let generatedTypes =
            protoFile.Messages
            |> Seq.map (TypeGeneration.createType rootScope lookup)
            |> Seq.iter container.AddMember
        provider
    
    let extractGeneratedTypes (pt: ProvidedTypeDefinition) =
       let rec loop (pt: Type) level =
           [
               for t in pt.GetNestedTypes() do
                   match t with 
                   | :? ProvidedUnion as pu ->
                       let parent =Some(pu.DeclaringType :?> ProvidedRecord)
                       yield GenerationType.ProvidedUnion(pu, parent)
                       
                   | :? ProvidedRecord as pr ->
                       yield! loop pr (level + 1)
                       let parent =
                           if level = 0 then None
                           else Some(pr.DeclaringType :?> ProvidedRecord)
                       yield GenerationType.ProvidedRecord(pr, parent)
                   | _ -> () //TODO: this would be enums or other types
           ]
       loop pt 0
           
                
    let createFSharpDefinitions(protoDef: string, outputFile, defaultnamespace) =    
        let config = TypeProviderConfig.makeConfig "resoultionfolder" "runtimeAssembly.dll" []
        use typeProviderForNamespaces = new TypeProviderForNamespaces(config)
        let providedTypeRoot = createProvidedTypes protoDef defaultnamespace
        //typeProviderForNamespaces.AddNamespace("", [provided] )
                
        let providedTypes = extractGeneratedTypes providedTypeRoot            
        //TODO: support enum types: enum types x.Field :?> Provided.Literal?
                
        let openSystem = SynModuleDecl.CreateOpen (LongIdentWithDots.CreateString "System")
        let openSystemCollectionsGeneric = SynModuleDecl.CreateOpen (LongIdentWithDots.CreateString "System.Collections.Generic")
        let openBinaryCodec = SynModuleDecl.CreateOpen (LongIdentWithDots.CreateString "Falanx.BinaryCodec")
        let openBinaryCodecPrimitive = SynModuleDecl.CreateOpen (LongIdentWithDots.CreateString "Falanx.BinaryCodec.Primitives")
        
        let knownNamespaces =
            [ providedTypeRoot.Namespace
              "System"
              "System.Collections.Generic"
              "Falanx.BinaryCodec"
              "Falanx.BinaryCodec.Primitives"
              "Microsoft.FSharp.Core"
              "Microsoft.FSharp.Core.Operators"
              "Microsoft.FSharp.Collections"
              "Microsoft.FSharp.Control"
              "Microsoft.FSharp.Text" ]
            |> Set.ofSeq
            
        let synTypes =
            providedTypes
            |> List.map (function
                         | ProvidedRecord(pr, parent) ->
                             SynModuleDecl.CreateRecord(pr, typeof<TypeContainer>, knownNamespaces)
                         | ProvidedUnion(pu, parent) ->
                             let union = SynModuleDecl.CreateUnion(pu, typeof<TypeContainer>, knownNamespaces)
                             match parent with 
                             | Some parent ->
                                 let info = SynComponentInfoRcd.Create(Ident.CreateLong parent.Name)
                                 let synModule = SynModuleDecl.CreateNestedModule(info, [union] )
                                 synModule
                             | None -> union )
                             
        let parseTree =
            ParsedInput.CreateImplFile(
                ParsedImplFileInputRcd.CreateFs(outputFile)
                    .AddModule(
                        SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong defaultnamespace)
                            .AddDeclarations ( [ yield openSystem
                                                 yield openSystemCollectionsGeneric
                                                 yield openBinaryCodec
                                                 yield openBinaryCodecPrimitive
                                                 yield! synTypes] )
                    )
            )
        let formattedCode = formatAst parseTree
        IO.File.WriteAllText(outputFile, formattedCode)
        #if DEBUG
        printfn "%s" formattedCode
        #endif       