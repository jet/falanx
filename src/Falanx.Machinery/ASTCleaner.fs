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
        | SynPat.LongIdent(longDotId, ident, synValTyparDecls,synConstructorArgs,accessibility,range) -> node
        | SynPat.Tuple(synPats, range) -> node
        | SynPat.StructTuple(synPats, range) -> node
        | SynPat.Paren(synPat, range) -> node 
        | SynPat.ArrayOrList(a, synPats, range) -> node
        | SynPat.Record (fields, range) -> node
        | SynPat.Null ( range) -> node
        | SynPat.OptionalVal(ident,range) -> node
        | SynPat.IsInst(synType,range) -> node
        | SynPat.QuoteExpr (synExpr, range) -> node
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
            
        | SynExpr.Quote(operator: SynExpr, isRaw: bool, quotedSynExpr: SynExpr, isFromQueryExpression: bool, range: range) -> node
        | SynExpr.Const(constant: SynConst, range: range) -> node
        | SynExpr.Typed( SynExpr.Record _ as record, typeName: SynType, range: range) -> record
        | SynExpr.Typed( expr: SynExpr, typeName: SynType, range: range) -> node
        | SynExpr.Tuple( (*isStruct: bool,*) exprs: SynExpr list, commaRanges: range list, range: range) -> node
        //| SynExpr.AnonRecd( isStruct: bool,  copyInfo:(SynExpr, BlockSeparator) option, recordFields:(Ident, SynExpr) list, range: range) -> node
        | SynExpr.ArrayOrList( isList: bool, exprs: SynExpr list, range: range) -> node
        | SynExpr.Record(baseInfo: (SynType * SynExpr * range * BlockSeparator option * range) option, copyInfo:(SynExpr * BlockSeparator) option, recordFields:(RecordFieldName * (SynExpr option) * BlockSeparator option) list, range: range) -> node
        | SynExpr.New(isProtected: bool, typeName: SynType, expr: SynExpr, range: range) -> node
        | SynExpr.ObjExpr(objType: SynType, argOptions:(SynExpr * Ident option) option, bindings: SynBinding list, extraImpls: SynInterfaceImpl list, newExprRange: range, range: range) -> node
        | SynExpr.While(whileSeqPoint: SequencePointInfoForWhileLoop, whileExpr: SynExpr, doExpr: SynExpr, range: range) -> node
        | SynExpr.For(forSeqPoint: SequencePointInfoForForLoop, ident: Ident, identBody: SynExpr, bool, toBody: SynExpr, doBody: SynExpr, range: range) -> node
        | SynExpr.ForEach(forSeqPoint: SequencePointInfoForForLoop, seqExprOnly: SeqExprOnly, isFromSource: bool, pat: SynPat, enumExpr: SynExpr, bodyExpr: SynExpr, range: range) -> node
        | SynExpr.ArrayOrListOfSeqExpr(isArray: bool, expr: SynExpr, range: range) -> node
        | SynExpr.CompExpr(isArrayOrList: bool, isNotNakedRefCell: bool ref, expr: SynExpr, range: range) -> node
        | SynExpr.MatchLambda(isExnMatch, range1, clauses, matchSeqpoint, range2) -> node
        | SynExpr.Match( matchSeqPoint, expr, clauses, isExnMatch, range) -> node
        | SynExpr.Do( expr: SynExpr, range: range) -> node
        | SynExpr.Assert(expr: SynExpr, range: range) -> node
        | SynExpr.TryWith(tryExpr: SynExpr, tryRange: range, withCases: SynMatchClause list, withRange: range, range: range, trySeqPoint: SequencePointInfoForTry, withSeqPoint: SequencePointInfoForWith) -> node
        | SynExpr.TryFinally(tryExpr: SynExpr, finallyExpr: SynExpr, range: range, trySeqPoint: SequencePointInfoForTry, finallySeqPoint: SequencePointInfoForFinally) -> node
        | SynExpr.Lazy(expr, range: range) -> node
        | SynExpr.IfThenElse(ifExpr: SynExpr, thenExpr: SynExpr, elseExpr: SynExpr option, spIfToThen: SequencePointInfoForBinding, isFromErrorRecovery: bool, ifToThenRange: range, range: range) -> node
        | SynExpr.Ident(ident) -> node
        | SynExpr.LongIdent(isOptional: bool, longDotId: LongIdentWithDots, altNameRefCell: SynSimplePatAlternativeIdInfo ref option, range: range) -> node
        | SynExpr.LongIdentSet(longDotId: LongIdentWithDots, expr: SynExpr, range: range) -> node
        | SynExpr.DotGet(expr: SynExpr, rangeOfDot: range, longDotId: LongIdentWithDots, range: range) -> node
        | SynExpr.DotSet(expr1, longDotId: LongIdentWithDots, expr2, range: range) -> node
        | SynExpr.Set(expr1, expr2, range: range) -> node
        | SynExpr.DotIndexedGet(expr, args, range1, range2) -> node
        | SynExpr.DotIndexedSet(objectExpr: SynExpr, indexExprs: SynIndexerArg list, valueExpr: SynExpr, leftOfSetRange: range, dotRange: range, range: range) -> node
        | SynExpr.NamedIndexedPropertySet(longDotId: LongIdentWithDots, expr1, expr2, range: range) -> node
        | SynExpr.DotNamedIndexedPropertySet(expr, longDotId: LongIdentWithDots, expr1, expr2, range: range) -> node
        | SynExpr.TypeTest( expr: SynExpr, typeName: SynType, range: range) -> node
        | SynExpr.Upcast( expr: SynExpr, typeName: SynType, range: range) -> node
        | SynExpr.Downcast( expr: SynExpr, typeName: SynType, range: range) -> node
        | SynExpr.InferredUpcast( expr: SynExpr, range: range) -> node
        | SynExpr.InferredDowncast( expr: SynExpr, range: range) -> node
        | SynExpr.Null(range: range) -> node
        | SynExpr.AddressOf( isByref: bool, expr, range1, range2: range) -> node
        | SynExpr.TraitCall(list, synMemberSig, expr, range: range) -> node
        | SynExpr.JoinIn(expr1, range1, expr2, range2: range) -> node
        | SynExpr.ImplicitZero(range: range) -> node
        | SynExpr.YieldOrReturn(thing1, expr: SynExpr, range: range) -> node
        | SynExpr.YieldOrReturnFrom(thing1, expr: SynExpr, range: range) -> node
        | SynExpr.LetOrUseBang(bindSeqPoint: SequencePointInfoForBinding, isUse: bool, isFromSource: bool, pat, expr1, expr2, range: range) -> node
        | SynExpr.MatchBang( matchSeqPoint, expr, clauses, isExnMatch, range2 ) -> node
        | SynExpr.DoBang(expr: SynExpr, range: range) -> node
        | SynExpr.LibraryOnlyILAssembly(ilInstr, types, exprs, types2, range: range ) -> node
        | SynExpr.LibraryOnlyStaticOptimization(list, expr1, expr2, range: range) -> node
        | SynExpr.LibraryOnlyUnionCaseFieldGet(expr: SynExpr, longId: LongIdent, int, range: range) -> node
        | SynExpr.LibraryOnlyUnionCaseFieldSet(expr1, longId: LongIdent, int, expr2, range: range) -> node
        | SynExpr.ArbitraryAfterError(debugStr: string, range: range) -> node
        | SynExpr.FromParseError(expr: SynExpr, range: range) -> node
        | SynExpr.DiscardAfterMissingQualificationAfterDot(SynExpr, range: range) -> node
        | SynExpr.Fixed(expr: SynExpr, range: range) -> node
         
    let untypeSynMemberDefn (node: SynMemberDefn) =
        match node with
        | SynMemberDefn.Member(binding, range) ->
            SynMemberDefn.Member(untypeSynBinding binding, range)
            
        | SynMemberDefn.LetBindings(bindings, isStatic, isRec, range) ->
             SynMemberDefn.LetBindings(bindings |> List.map untypeSynBinding, isStatic, isRec, range)
             
        | other -> other
     
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
        | other -> other
        
    let untypeSynModuleDecls (nodes: SynModuleDecls) =
        nodes |> List.map untypeSynModuleDecl
    
    let unTypeSynModuleOrNamespace (SynModuleOrNamespace.SynModuleOrNamespace(longident, isRecursive, isModule, moduleDecls, preXmlDoc, attributes, access, range)) =
       let cleanModules = untypeSynModuleDecls moduleDecls
       SynModuleOrNamespace(longident, isRecursive, isModule, cleanModules, preXmlDoc, attributes, access, range)
        
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
            
