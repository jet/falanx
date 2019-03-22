namespace Falanx.Machinery

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Utils
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open ProviderImplementation.ProvidedTypes

module ASTCleaner =
  
    let rec untypeSimplePat (pat: SynSimplePat) =
        match pat with
        | SynSimplePat.Typed(simplePat, typ, range) ->
            untypeSimplePat simplePat
            
        | SynSimplePat.Id(ident, altName, generated, isVar, isOptArg,range) ->
            SynSimplePat.Id(ident, altName, generated, isVar, isOptArg, range)
            
        | SynSimplePat.Attrib(simplePat, attribs, range) ->
            SynSimplePat.Attrib(untypeSimplePat simplePat, attribs, range)
          
    let rec untypeSimplePats (patt: SynSimplePats) =
        match patt with
        | SynSimplePats.Typed(simplePats, typ, range) ->
             untypeSimplePats simplePats
             
        | SynSimplePats.SimplePats(patts, range) ->
            SynSimplePats.SimplePats(patts |> List.map untypeSimplePat, range)
    
    let rec untypeSynBinding (SynBinding.Binding(access, bindingKind, mustInline, isMutable, attributes, preXmlDoc, valData, pat, returnInfo, expr, range, l)) =
        SynBinding.Binding(access, bindingKind, mustInline, isMutable, attributes, preXmlDoc, valData, untypeSynPat pat, returnInfo, untypeSynExpr expr, range, l)
    
    and untypeSynPat (node: SynPat) =
        match node with
        | SynPat.Const(synConst, range) -> node
        | SynPat.Wild(range) -> node
        | SynPat.Named(synPat,ident,isSelfIdentifier,accessibility,range) -> node
        | SynPat.Typed(synPat, synType, range) -> synPat
        | SynPat.Attrib(synPat, synAttributes, range) -> node
        | SynPat.Or(synPatL, synPatR, range_)-> node
        | SynPat.Ands(synPats,  range) -> node
        | SynPat.LongIdent(longDotId, ident, synValTyparDecls, synConstructorArgs, accessibility, range) -> node
        | SynPat.Tuple(synPats, range) -> node
        | SynPat.StructTuple(synPats, range) -> node
        | SynPat.Paren(synPat, range) -> node 
        | SynPat.ArrayOrList(a, synPats, range) -> node
        | SynPat.Record(fields, range) -> node
        | SynPat.Null(range) -> node
        | SynPat.OptionalVal(ident,range) -> node
        | SynPat.IsInst(synType,range) -> node
        | SynPat.QuoteExpr(synExpr, range) -> node
        | SynPat.DeprecatedCharRange(char1, char2, range) -> node
        | SynPat.InstanceMember(ident1, ident2, ident3, accessibility,range) -> node
        | SynPat.FromParseError(synPat, range) -> node

    and untypeSynExpr (node: SynExpr) =
        match node with
        | SynExpr.Paren (expr, leftParenRange, rightParenRange, range) ->
            SynExpr.Paren(untypeSynExpr expr, leftParenRange, rightParenRange, range)
        
        | SynExpr.Lambda( fromMethod, inLambdaSeq, args, body, range) ->
            SynExpr.Lambda(fromMethod, inLambdaSeq, untypeSimplePats args, untypeSynExpr body, range)
                
        | SynExpr.App(exprAtomicFlag, isInfix, funcExpr, argExpr, range) ->
            SynExpr.App(exprAtomicFlag, isInfix, untypeSynExpr funcExpr, untypeSynExpr argExpr, range)
          
        | SynExpr.TypeApp(expr, lessRange, typeNames, commaRanges, greaterRange, typeArgsRange, range) ->
            SynExpr.TypeApp(expr, lessRange, typeNames, commaRanges, greaterRange, typeArgsRange, range)
            
        | SynExpr.LetOrUse(isRecursive, isUse, bindings, body, range) ->
            SynExpr.LetOrUse(isRecursive,isUse, bindings |> List.map untypeSynBinding, untypeSynExpr body,range)
            
        | SynExpr.Sequential(seqPoint, isTrueSeq, expr1, expr2, range) ->
            SynExpr.Sequential(seqPoint, isTrueSeq, untypeSynExpr expr1, untypeSynExpr expr2, range)
            
        | SynExpr.Typed( SynExpr.Record _ as record, typeName: SynType, range) ->
            record
        
        | SynExpr.Quote(operator, isRaw, quotedSynExpr, isFromQueryExpression, range) -> node
        | SynExpr.Const(constant, range) -> node
        | SynExpr.Typed(expr, typeName, range) -> node
        | SynExpr.Tuple( (*isStruct: bool,*) exprs, commaRanges, range) -> node
        | SynExpr.StructTuple(exprs, commaRanges, range) -> node
        //| SynExpr.AnonRecd( isStruct: bool,  copyInfo:(SynExpr, BlockSeparator) option, recordFields:(Ident, SynExpr) list, range) -> node
        | SynExpr.ArrayOrList(isList, exprs, range) -> node
        | SynExpr.Record(baseInfo, copyInfo, recordFields, range) -> node
        | SynExpr.New(isProtected, typeName, expr, range) -> node
        | SynExpr.ObjExpr(objType, argOptions, bindings, extraImpls, newExprRange, range) -> node
        | SynExpr.While(whileSeqPoint, whileExpr, doExpr, range) -> node
        | SynExpr.For(forSeqPoint, ident, identBody, bool, toBody, doBody, range) -> node
        | SynExpr.ForEach(forSeqPoint, seqExprOnly, isFromSource, pat, enumExpr, bodyExpr, range) -> node
        | SynExpr.ArrayOrListOfSeqExpr(isArray, expr, range) -> node
        | SynExpr.CompExpr(isArrayOrList, isNotNakedRefCell, expr, range) -> node
        | SynExpr.MatchLambda(isExnMatch, range1, clauses, matchSeqpoint, range2) -> node
        | SynExpr.Match(matchSeqPoint, expr, clauses, isExnMatch, range) -> node
        | SynExpr.Do(expr, range) -> node
        | SynExpr.Assert(expr, range) -> node
        | SynExpr.TryWith(tryExpr, tryRange, withCases, withRange, range, trySeqPoint, withSeqPoint) -> node
        | SynExpr.TryFinally(tryExpr, finallyExpr, range, trySeqPoint, finallySeqPoint) -> node
        | SynExpr.Lazy(expr, range) -> node
        | SynExpr.IfThenElse(ifExpr, thenExpr, elseExpr, spIfToThen, isFromErrorRecovery, ifToThenRange, range) -> node
        | SynExpr.Ident(ident) -> node
        | SynExpr.LongIdent(isOptional, longDotId, altNameRefCell, range) -> node
        | SynExpr.LongIdentSet(longDotId, expr, range) -> node
        | SynExpr.DotGet(expr, rangeOfDot, longDotId, range) -> node
        | SynExpr.DotSet(expr1, longDotId, expr2, range) -> node
        | SynExpr.Set(expr1, expr2, range) -> node
        | SynExpr.DotIndexedGet(expr, args, range1, range2) -> node
        | SynExpr.DotIndexedSet(objectExpr, indexExprs, valueExpr, leftOfSetRange, dotRange, range) -> node
        | SynExpr.NamedIndexedPropertySet(longDotId, expr1, expr2, range) -> node
        | SynExpr.DotNamedIndexedPropertySet(expr, longDotId, expr1, expr2, range) -> node
        | SynExpr.TypeTest(expr, typeName, range) -> node
        | SynExpr.Upcast(expr, typeName, range) -> node
        | SynExpr.Downcast(expr, typeName, range) -> node
        | SynExpr.InferredUpcast(expr, range) -> node
        | SynExpr.InferredDowncast(expr, range) -> node
        | SynExpr.Null(range) -> node
        | SynExpr.AddressOf( isByref, expr, range1, range2) -> node
        | SynExpr.TraitCall(list, synMemberSig, expr, range) -> node
        | SynExpr.JoinIn(expr1, range1, expr2, range2) -> node
        | SynExpr.ImplicitZero(range) -> node
        | SynExpr.YieldOrReturn(thing1, expr, range) -> node
        | SynExpr.YieldOrReturnFrom(thing1, expr, range) -> node
        | SynExpr.LetOrUseBang(bindSeqPoint, isUse, isFromSource, pat, expr1, expr2, range) -> node
        | SynExpr.MatchBang( matchSeqPoint, expr, clauses, isExnMatch, range2) -> node
        | SynExpr.DoBang(expr, range) -> node
        | SynExpr.LibraryOnlyILAssembly(ilInstr, types, exprs, types2, range) -> node
        | SynExpr.LibraryOnlyStaticOptimization(list, expr1, expr2, range) -> node
        | SynExpr.LibraryOnlyUnionCaseFieldGet(expr, longId, int, range) -> node
        | SynExpr.LibraryOnlyUnionCaseFieldSet(expr1, longId, int, expr2, range) -> node
        | SynExpr.ArbitraryAfterError(debugStr, range) -> node
        | SynExpr.FromParseError(expr, range) -> node
        | SynExpr.DiscardAfterMissingQualificationAfterDot(expr, range) -> node
        | SynExpr.Fixed(expr, range) -> node
         
    let untypeSynMemberDefn (node: SynMemberDefn) =
        match node with
        | SynMemberDefn.Member(binding, range) ->
            SynMemberDefn.Member(untypeSynBinding binding, range)
            
        | SynMemberDefn.LetBindings(bindings, isStatic, isRec, range) ->
             SynMemberDefn.LetBindings(bindings |> List.map untypeSynBinding, isStatic, isRec, range)
             
        | SynMemberDefn.Open(longId, range) -> node
        | SynMemberDefn.ImplicitCtor(accessiblity, attributes, ctorArgs, selfIdentifier, range) -> node
        | SynMemberDefn.ImplicitInherit(inheritType, inheritArgs, inheritAlias, range) -> node
        | SynMemberDefn.AbstractSlot(synValSig, memberFlags, range) -> node
        | SynMemberDefn.Interface(typ, memberDefns, range) -> node
        | SynMemberDefn.Inherit(typ, ident, range) -> node
        | SynMemberDefn.ValField(field, range) -> node
        | SynMemberDefn.NestedType(typeDefn, accessibility, range) -> node
        | SynMemberDefn.AutoProperty(attribs, isStatic, ident, typeOpt, propKind, memberFlags, xmlDoc, accessiblity, synExpr, getSetRange, range) -> node
         
    let untypeSynTypeDefn (SynTypeDefn.TypeDefn(ci, typeDefRepr, synMemberDefns, range)) =
         TypeDefn(ci, typeDefRepr, synMemberDefns |> List.map untypeSynMemberDefn, range)
        
    let rec untypeSynModuleDecl (node: SynModuleDecl) =
        match node with
        | SynModuleDecl.Let(a,bindings,range) ->
            SynModuleDecl.Let(a, bindings |> List.map untypeSynBinding, range)
            
        | SynModuleDecl.DoExpr(a,expr,c) ->
            SynModuleDecl.DoExpr(a, untypeSynExpr expr,c)
            
        | SynModuleDecl.Types(types, range) ->
            SynModuleDecl.Types(types |> List.map untypeSynTypeDefn , range)
            
        | SynModuleDecl.NestedModule(synComponentInfo, isRecursive, synModuleDecls, bool, range) ->
            SynModuleDecl.NestedModule(synComponentInfo, isRecursive, synModuleDecls |> List.map untypeSynModuleDecl, bool, range)
        
        | SynModuleDecl.ModuleAbbrev(ident: Ident, longId: LongIdent, range) -> node
        | SynModuleDecl.Exception(synExceptionDefn, range) -> node
        | SynModuleDecl.Open(longDotId: LongIdentWithDots, range) -> node
        | SynModuleDecl.Attributes(synAttributes, range) -> node
        | SynModuleDecl.HashDirective(parsedHashDirective, range) -> node
        | SynModuleDecl.NamespaceFragment(synModuleOrNamespace) -> node
        
    let untypeSynModuleDecls (nodes: SynModuleDecls) =
        nodes |> List.map untypeSynModuleDecl
    
    let unTypeSynModuleOrNamespace (SynModuleOrNamespace.SynModuleOrNamespace(longident, isRecursive, isModule, moduleDecls, preXmlDoc, attributes, access, range)) =
       SynModuleOrNamespace(longident, isRecursive, isModule, untypeSynModuleDecls moduleDecls, preXmlDoc, attributes, access, range)
        
    let unTypeSynModuleOrNamespaces (nodes: SynModuleOrNamespace list) =
        nodes |> List.map unTypeSynModuleOrNamespace

    let untypeParseTree (node: Microsoft.FSharp.Compiler.Ast.ParsedInput) : ParsedInput =
        match node with
        | ParsedInput.SigFile sign -> ParsedInput.SigFile sign
        | ParsedInput.ImplFile impFile ->
            match impFile with
            | ParsedImplFileInput.ParsedImplFileInput(name, isScript, qualifiedNameOfFile, scopedPragmas, hashDirectives, modules , g) ->
                let cleanModulesOrNamespaces = unTypeSynModuleOrNamespaces modules
                ParsedInput.ImplFile (ParsedImplFileInput(name, isScript, qualifiedNameOfFile, scopedPragmas, hashDirectives, cleanModulesOrNamespaces, g))
            
