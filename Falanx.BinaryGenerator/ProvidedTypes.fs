namespace Falanx.BinaryGenerator
module ProvidedTypes =
    open System
    open ProviderImplementation.ProvidedTypes
    open Microsoft.FSharp.Quotations
    open Falanx.Ast
    open Falanx.Ast.Prelude
    open Falanx.Ast.Expr
    
    let message name = ProvidedTypeDefinition(name, Some typeof<obj>, isErased = false)
    
    let enum name =
        let pt = ProvidedTypeDefinition(name, Some typeof<System.Enum>, isErased = false)
        pt.SetEnumUnderlyingType(typeof<int>) //REVIEW: Should this be generalized for non Int32 enums?
        pt

    let union name members = ProvidedTypeDefinition(name, Some typeof<obj>, isErased = false)
    
    let addEnumValues (enum: ProvidedTypeDefinition) =
        Seq.map(fun (name, value) -> ProvidedField.Literal(name, typeof<int>, value))
        >> Seq.iter enum.AddMember
        
    let propertyWithField propertyType name readonly =
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