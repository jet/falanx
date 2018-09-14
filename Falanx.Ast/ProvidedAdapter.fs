namespace Falanx.Ast
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes

module ProvidedMethod =
    open Utils

    let toExpr (m:ProvidedMethod) =
                            
        let parameters=
            [  if not m.IsStatic then
                   yield Expr.Var <| Var(thisPrefix, m.DeclaringType)
                    
               for p in m.GetParameters() do
                   yield Expr.Var <| Var(p.Name, p.ParameterType) ]
    
        m.GetInvokeCode |> function Some ik -> ik parameters | _ -> <@@ () @@>
    
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
  
module TypeProviderConfig =                                
    let makeConfig resolutionFolder runtimeAssembly runtimeAssemblyRefs =
         ProviderImplementation.ProvidedTypesTesting.Testing.MakeSimulatedTypeProviderConfig (resolutionFolder, runtimeAssembly, runtimeAssemblyRefs)
