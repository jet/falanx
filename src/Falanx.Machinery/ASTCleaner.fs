namespace Falanx.Machinery

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

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
        | SynPat.Const(_synConst, _range) -> node
        | SynPat.Wild(_range) -> node
        | SynPat.Named(_synPat, _ident, _isSelfIdentifier, _accessibility, _range) -> node
        | SynPat.Typed(synPat, _synType, _range) -> synPat
        | SynPat.Attrib(_synPat, _synAttributes, _range) -> node
        | SynPat.Or(_synPatL, _synPatR, _range_)-> node
        | SynPat.Ands(_synPats,  _range) -> node
        | SynPat.LongIdent(_longDotId, _ident, _synValTyparDecls, _synConstructorArgs, _accessibility, _range) -> node
        | SynPat.Tuple(_synPats, _range) -> node
        | SynPat.StructTuple(_synPats, _range) -> node
        | SynPat.Paren(_synPat, _range) -> node 
        | SynPat.ArrayOrList(_arrayOrList, _synPats, _range) -> node
        | SynPat.Record(_fields, _range) -> node
        | SynPat.Null(_range) -> node
        | SynPat.OptionalVal(_ident,_range) -> node
        | SynPat.IsInst(_synType, _range) -> node
        | SynPat.QuoteExpr(_synExpr, _range) -> node
        | SynPat.DeprecatedCharRange(_char1, _char2, _range) -> node
        | SynPat.InstanceMember(_ident1, _ident2, _ident3, _accessibility, _range) -> node
        | SynPat.FromParseError(_synPat, _range) -> node

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
            
        | SynExpr.Typed( SynExpr.Record _ as record, typeName, range) ->
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
             
        | SynMemberDefn.Open(_longId, _range) -> node
        | SynMemberDefn.ImplicitCtor(_accessiblity, _attributes, _ctorArgs, _selfIdentifier, _range) -> node
        | SynMemberDefn.ImplicitInherit(_inheritType, _inheritArgs, _inheritAlias, _range) -> node
        | SynMemberDefn.AbstractSlot(_synValSig, _memberFlags, _range) -> node
        | SynMemberDefn.Interface(_typ, _memberDefns, _range) -> node
        | SynMemberDefn.Inherit(_typ, _ident, _range) -> node
        | SynMemberDefn.ValField(_field, _range) -> node
        | SynMemberDefn.NestedType(_typeDefn, _accessibility, _range) -> node
        | SynMemberDefn.AutoProperty(_attribs, _isStatic, _ident, _typeOpt, _propKind, _memberFlags, _xmlDoc, _accessiblity, _synExpr, _getSetRange, _range) -> node
         
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
        
        | SynModuleDecl.ModuleAbbrev(_ident, _longId, _range) -> node
        | SynModuleDecl.Exception(_synExceptionDefn, _range) -> node
        | SynModuleDecl.Open(_longDotId, _range) -> node
        | SynModuleDecl.Attributes(_synAttributes, _range) -> node
        | SynModuleDecl.HashDirective(_parsedHashDirective, _range) -> node
        | SynModuleDecl.NamespaceFragment(_synModuleOrNamespace) -> node
        
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
            
