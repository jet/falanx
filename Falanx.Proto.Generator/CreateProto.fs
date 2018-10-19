namespace Falanx.Proto.Generator

type TypeContainer = class end
    
module Proto =
    open Falanx.Ast
    open System
    open Microsoft.FSharp.Compiler.Ast
    open Froto.Parser.ClassModel
    open FsAst
    open ProviderImplementation.ProvidedTypes
    
    let createProvidedTypes protoDef defaultnamespace =
        let protoFile = ProtoFile.fromString protoDef
                                    
        let rootScope = protoFile.Packages |> Seq.tryHead |> Option.defaultValue defaultnamespace
    
        let provider = 
            ProvidedTypeDefinition(
                Reflection.Assembly.GetCallingAssembly(),
                rootScope,
                typeof<TypeContainer>.Name,
                Some typeof<obj>, 
                hideObjectMethods = true, 
                isErased = false)
                
        let container = provider 
    
        let lookup = TypeResolver.discoverTypes rootScope protoFile
        
        protoFile.Enums
        |> Seq.map (TypeGeneration.createEnum rootScope lookup)
        |> Seq.iter container.AddMember
        
        let generatedTypes =
            protoFile.Messages
            |> Seq.map (TypeGeneration.createType rootScope lookup)
            |> Seq.iter container.AddMember
        provider
    
    let createFSharpDefinitions(protoDef: string, outputFile, defaultnamespace) =    
        let config = TypeProviderConfig.makeConfig "resoultionfolder" "runtimeAssembly.dll" []
        use typeProviderForNamespaces = new TypeProviderForNamespaces(config)
        let providedTypeRoot = createProvidedTypes protoDef defaultnamespace
                
        let openSystem = SynModuleDecl.CreateOpen (LongIdentWithDots.CreateString "System")
        let openFrotoSerialization = SynModuleDecl.CreateOpen (LongIdentWithDots.CreateString "Froto.Serialization")
        let openSystemCollectionsGeneric = SynModuleDecl.CreateOpen (LongIdentWithDots.CreateString "System.Collections.Generic")
        let openBinaryCodec = SynModuleDecl.CreateOpen (LongIdentWithDots.CreateString "Falanx.BinaryCodec")
        let openBinaryCodecPrimitive = SynModuleDecl.CreateOpen (LongIdentWithDots.CreateString "Falanx.BinaryCodec.Primitives")
        
        let knownNamespaces =
            [ providedTypeRoot.Namespace
              "System"
              "Froto.Serialization"
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
            let rec loop (pt: Type) =
                [
                    for t in pt.GetNestedTypes() do
                        match t with 
                        | :? ProvidedUnion as pu ->
                            yield SynModuleDecl.CreateUnion(pu, typeof<TypeContainer>, knownNamespaces)
                        | :? ProvidedRecord as pr ->
                            match loop t with 
                            | [] -> ()
                            | children -> 
                                let info = SynComponentInfoRcd.Create(Ident.CreateLong t.Name)
                                yield SynModuleDecl.CreateNestedModule(info, children)
                            yield SynModuleDecl.CreateRecord(pr, typeof<TypeContainer>, knownNamespaces)
                        | :? ProvidedTypeDefinition as pe when pe.IsEnum -> 
                            yield SynModuleDecl.CreateEnum(pe)
                        | _ -> () 
                ]
            loop providedTypeRoot
                             
        let parseTree =
            ParsedInput.CreateImplFile(
                ParsedImplFileInputRcd.CreateFs(outputFile)
                    .AddModule(
                        {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong providedTypeRoot.Namespace) with IsRecursive = true}
                            .AddDeclarations ( [ yield openSystem
                                                 yield openFrotoSerialization
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