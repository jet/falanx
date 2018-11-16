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
    
    let rec untypeSynBinding (SynBinding.Binding(a, b, c, d, e, f, g, h, i, expr, range, l)) =
        SynBinding.Binding(a, b, c, d, e, f, g, h, i, untypeSynExpr expr, range, l)
    
    and untypeSynExpr (node: SynExpr) =
        match node with
        | SynExpr.Lambda(fromMeth, inSeq, args, body, range) ->
            SynExpr.Lambda(fromMeth, inSeq, untypeSimplePats args, untypeSynExpr body, range)
            
        | SynExpr.App(atomic, isInfix, expr, expr2, range) ->
            SynExpr.App(atomic, isInfix, untypeSynExpr expr, untypeSynExpr expr2, range)
            
        | SynExpr.TypeApp(expr, lessRange, typeNames, commaRanges, greaterRange, typeArgsRange, range) ->
            //SynExpr.TypeApp(untypeSynExpr expr, lessRange, typeNames, commaRanges, greaterRange, typeArgsRange,range)
            untypeSynExpr expr
            
        | SynExpr.LetOrUse(a,b,bindings,expr,range) ->
            SynExpr.LetOrUse(a,b, bindings |> List.map untypeSynBinding, untypeSynExpr expr,range)
            
        | a -> a
    

     
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
            
