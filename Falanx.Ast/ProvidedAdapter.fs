namespace Falanx.Ast
open System
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open Falanx.Ast.Prelude
open Falanx.Ast.Expr

[<RequireQualifiedAccess>]
module ProvidedMethod =
    open Utils

    let toExpr (m:ProvidedMethod) =
                            
        let parameters=
            [  if not m.IsStatic then
                   yield Expr.Var <| Var(thisPrefix, m.DeclaringType)
                    
               for p in m.GetParameters() do
                   yield Expr.Var <| Var(p.Name, p.ParameterType) ]
    
        m.GetInvokeCode |> function Some ik -> ik parameters | _ -> <@@ () @@>

[<RequireQualifiedAccess>]    
module ProvidedTypeDefinition =
    let getXmlDocs (providedType: ProvidedTypeDefinition) =
        providedType.GetCustomAttributesData()
        |> Seq.choose (fun cad -> if cad.AttributeType = typeof<CompilerServices.TypeProviderXmlDocAttribute> 
                                  then Some(cad.ConstructorArguments.[0].Value :?> string)
                                  else None)
                                  
    let getMethodOverridesByInterfaceType(providedType: ProvidedTypeDefinition) =
        let overrides = providedType.GetMethodOverrides()
        let grouped = overrides |> Array.groupBy (fun (_pm, mi) -> mi.DeclaringType)
        grouped
        
    let mkEnum name =
        let pt = ProvidedTypeDefinition(name, Some typeof<System.Enum>, isErased = false)
        pt.SetEnumUnderlyingType(typeof<int>)
        pt
    
    let addEnumValues (enum: ProvidedTypeDefinition) =
        Seq.map(fun (name, value) -> ProvidedField.Literal(name, typeof<int>, value))
        >> Seq.iter enum.AddMember
        
    let mkPropertyWithField propertyType name readonly =
        let field = ProvidedField(Naming.pascalToCamel name, propertyType)
        field.SetFieldAttributes(Reflection.FieldAttributes.InitOnly ||| Reflection.FieldAttributes.Private)
        let property = 
            ProvidedProperty(
                name, 
                propertyType, 
                getterCode = (fun args -> Expr.FieldGet(args.[0], field)),
                ?setterCode =
                    if readonly then None
                    else Some(fun args ->
                        let setter = Expr.FieldSet(args.[0], field, args.[1])
                        if propertyType.IsValueType || 
                            // None appears to be represented as null.
                            (propertyType.IsGenericType && propertyType.GetGenericTypeDefinition() = typedefof<option<_>>)
                        then setter
                        else
                            Expr.Sequential(
                                <@@ argNotNull x x @@>
                                |> Expr.methoddefof
                                |> Expr.callStatic [Expr.Value name; Expr.box args.[1]],
                                setter)))
    
        property, field   
  
module TypeProviderConfig =                                
    let makeConfig resolutionFolder runtimeAssembly runtimeAssemblyRefs =
         ProviderImplementation.ProvidedTypesTesting.Testing.MakeSimulatedTypeProviderConfig (resolutionFolder, runtimeAssembly, runtimeAssemblyRefs)