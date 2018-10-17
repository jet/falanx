namespace Falanx.Ast
    [<AutoOpen>]
    module SynTypeExtensions =
    
        open System
        open FsAst
        open Microsoft.FSharp.Compiler.Ast
        open Microsoft.FSharp.Compiler.Range
        open ProviderImplementation.ProvidedTypes
        open Utils
        open Prelude
        open Falanx.Ast.ProvidedTypesExtension
        
        type SynType with
            static member CreateFromType(typ: Type, ?ommitEnclosingType, ?knownNamespaces, ?isPostfix) =       
                let isPostfix = defaultArg isPostfix true
                if typ.IsGenericType then
                    let genericParams =
                        typ.GetGenericArguments()
                        |> Seq.map 
                            (fun gt -> 
                                if gt.IsGenericType then 
                                    SynType.CreateFromType(gt, ?ommitEnclosingType = ommitEnclosingType, ?knownNamespaces = knownNamespaces, isPostfix = false)
                                else
                                    let name = TypeHelpers.FormatType(false,false, gt, ommitEnclosingType = ommitEnclosingType, ?knownNamespaces = knownNamespaces)
                                    name
                                    |> LongIdentWithDots.CreateString
                                    |> SynType.CreateLongIdent ) 
                        |> Seq.toList
                        
                    let genericType =
                        TypeHelpers.FormatType(false ,true, typ.GetGenericTypeDefinition(), ommitEnclosingType = ommitEnclosingType, ?knownNamespaces = knownNamespaces)
                        |> LongIdentWithDots.CreateString
                        |> SynType.CreateLongIdent
                    
                    SynType.CreateApp(genericType, genericParams, isPostfix)
                    
                else
                    SynType.CreateLongIdent(LongIdentWithDots.CreateString (TypeHelpers.FormatType(false,false, typ, ommitEnclosingType = ommitEnclosingType, ?knownNamespaces = knownNamespaces) ) )
                   
        type SynMemberDefn with
                static member  CreateFromProvidedMethod (pm:ProvidedMethod, ?ommitEnclosingType : Type, ?knownNamespaces: _ Set) =
                    let ident =
                        let ident = if pm.IsStatic then pm.Name else (thisPrefix +.+ pm.Name)
                        LongIdentWithDots.CreateString ident
                        
                    let synExpr, _parseTree =
                        Quotations.ToAst(ProvidedMethod.toExpr pm, ?ommitEnclosingType = ommitEnclosingType, ?knownNamespaces = knownNamespaces)
                    
                    let flags =
                        if pm.IsStatic
                        then Some MemberFlags.StaticMember
                        else Some MemberFlags.InstanceMember
                        
                    let parameters =
                        pm.GetParameters()
                        |> Seq.choose (function :? ProvidedParameter as p -> Some p | _ -> None )
                        |> Seq.map (fun pp -> SynPatRcd.CreateTyped(SynPatRcd.CreateNamed(Ident.Create pp.Name, SynPatRcd.CreateWild), SynType.CreateFromType(pp.ParameterType, ?ommitEnclosingType = ommitEnclosingType, ?knownNamespaces = knownNamespaces)))
                        |> Seq.toList
                       
                    SynMemberDefn.CreateMember
                        { SynBindingRcd.Null with
                            Pattern = SynPatRcd.CreateLongIdent(ident, [ SynPatRcd.CreateParen(SynPatRcd.CreateTuple parameters) ] )
                            Expr = synExpr
                            ValData = SynValData(flags, SynValInfo.Empty, None)
                        }
        type SynFieldRcd with               
            static member CreateFromPropertyInfo(pp: Reflection.PropertyInfo, isMutable, ?ommitEnclosingType) =
                let typeName = SynType.CreateFromType(pp.PropertyType, ?ommitEnclosingType = ommitEnclosingType)
                SynFieldRcd.Create(Ident.Create(pp.Name), typeName, isMutable)
        
        type SynModuleDecl with
            static member CreateRecord (pt: ProvidedRecord, ?ommitEnclosingType, ?knownNamespaces) =
                let recordFields =
                    let props =
                        pt.GetProperties()
                        |> Seq.choose (function :? ProvidedProperty as pp -> Some pp | _ -> None)
                        |> Seq.map (fun pp -> SynFieldRcd.CreateFromPropertyInfo(pp, true, ?ommitEnclosingType = ommitEnclosingType))
                        |> Seq.toList
                    props
                    
                let interfacesAndMembers = ProvidedTypeDefinition.getMethodOverridesByInterfaceType pt
                let membersInInterfaces = interfacesAndMembers |> Array.collect (fun (_, m) -> m |> Array.map fst ) |> ResizeArray
                    
                let interfaces =
                    [ for (interface', members) in interfacesAndMembers do
                          let methods =
                              members
                              |> Array.map (fun (pm, _mi) -> SynMemberDefn.CreateFromProvidedMethod(pm, ?ommitEnclosingType = ommitEnclosingType, ?knownNamespaces = knownNamespaces))
                              |> Array.toList                                                                  
                          yield SynMemberDefn.CreateInterface(SynType.CreateFromType(interface', ?knownNamespaces = knownNamespaces), Some methods)
                    ]
                
                let staticMethods =
                    pt.GetMethods()
                    |> Seq.choose(fun pm ->  match pm with
                                             | :? ProvidedMethod as pm when pm.IsStatic ->
                                                 let name = pm.Name
                                                 if name.StartsWith("get_") || name.StartsWith("set_")
                                                 then None
                                                 else Some(SynMemberDefn.CreateFromProvidedMethod(pm, ?ommitEnclosingType = ommitEnclosingType, ?knownNamespaces = knownNamespaces))
                                             | _ -> None)
                    |> Seq.toList
                    
                let instanceMethodsNotInInterface =
                    pt.GetMethods()
                    |> Seq.choose(fun pm ->  match pm with
                                             | :? ProvidedMethod as pm when not pm.IsStatic && not (membersInInterfaces.Contains pm)  ->
                                                 let name = pm.Name
                                                 if name.StartsWith("get_") || name.StartsWith("set_")
                                                 then None
                                                 else Some(SynMemberDefn.CreateFromProvidedMethod(pm, ?ommitEnclosingType = ommitEnclosingType, ?knownNamespaces = knownNamespaces))
                                             | _ -> None)
                    |> Seq.toList
                    
                let attributes =
                    let cliMutableAttribute =
                        SynModuleDecl.CreateAttribute(LongIdentWithDots.CreateString("CLIMutable"),  SynExpr.CreateConst SynConst.Unit, false)
                    [cliMutableAttribute]
                   
                SynModuleDecl.CreateSimpleType (
                    { SynComponentInfoRcd.Create (Ident.CreateLong pt.Name) with
                          XmlDoc = PreXmlDoc.Create (ProvidedTypeDefinition.getXmlDocs pt)
                          Attributes = attributes },
                    SynTypeDefnSimpleReprRecordRcd.Create(recordFields) |> SynTypeDefnSimpleReprRcd.Record,
                    members = [ yield! staticMethods
                                yield! instanceMethodsNotInInterface
                                yield! interfaces ]
                )
                
            static member CreateUnion(pu: ProvidedUnion, ?ommitEnclosingType, ?knownNamespaces) =
                let unionCases =
                    pu.UnionCases
                    |> Array.map (fun uc -> let unionFields =
                                                uc.fields
                                                |> List.map (fun pi -> SynFieldRcd.CreateFromPropertyInfo(pi, false, ?ommitEnclosingType = ommitEnclosingType))
                                            let unioncaseType = SynUnionCaseType.Create(unionFields)
                                            SynUnionCaseRcd.Create(Ident.Create uc.name, unioncaseType) )
                    |> Array.toList
        
                SynModuleDecl.CreateSimpleType (
                    { SynComponentInfoRcd.Create (Ident.CreateLong pu.Name) with
                          XmlDoc = PreXmlDoc.Create (ProvidedTypeDefinition.getXmlDocs pu) },
                    SynTypeDefnSimpleReprUnionRcd.Create(unionCases) |> SynTypeDefnSimpleReprRcd.Union,
                    members = []
                )

            static member CreateEnum(pe: ProvidedTypeDefinition) =
                let synConst =    
                    let t = pe.GetEnumUnderlyingType()
                    let case = 
                        FSharp.Reflection.FSharpType.GetUnionCases(typeof<SynConst>)
                        |> Seq.find 
                            (fun x -> 
                                let fs = x.GetFields()
                                fs.Length > 0 && fs.[0].PropertyType = t
                            )
                    fun v -> FSharp.Reflection.FSharpValue.MakeUnion(case,[|v|]) :?> SynConst
                let enumCases =
                    pe.GetFields()
                    |> Seq.filter (fun fi -> fi.IsLiteral)
                    |> Seq.map (fun fi -> SynEnumCaseRcd.Create(Ident.Create fi.Name, synConst(fi.GetRawConstantValue())))
                    |> Seq.toList
        
                SynModuleDecl.CreateSimpleType (
                    { SynComponentInfoRcd.Create (Ident.CreateLong pe.Name) with
                          XmlDoc = PreXmlDoc.Create (ProvidedTypeDefinition.getXmlDocs pe) },
                    SynTypeDefnSimpleReprEnumRcd.Create(enumCases) |> SynTypeDefnSimpleReprRcd.Enum,
                    members = []
                )