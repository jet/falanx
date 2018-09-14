namespace Falanx.Ast.ProvidedTypesExtension
open System
open System.Collections.Generic
open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Falanx.Ast.Reflection
                
type ProvidedUnionCase =
    { tag : int 
      name : string
      position : int
      declaringType : Type
      fields: PropertyInfo list
      unionCaseType: ProvidedTypeDefinition }  

type ProvidedUnion(isTgt: bool, container:TypeContainer, className: string, getBaseType: (unit -> Type option), attrs: TypeAttributes, getEnumUnderlyingType, staticParams, staticParamsApply, backingDataSource, customAttributesData, nonNullable, hideObjectMethods) =
    inherit ProvidedTypeDefinition(isTgt, container, className, getBaseType, attrs,  getEnumUnderlyingType, staticParams, staticParamsApply, backingDataSource, customAttributesData, nonNullable, hideObjectMethods)
    let tagsType =
        let tags = ProvidedTypeDefinition("Tags", Some typeof<obj>, isErased = false)
        tags.SetAttributes (TypeAttributes.Public ||| TypeAttributes.Class ||| TypeAttributes.Sealed ||| TypeAttributes.Abstract)
        tags
  
    let unionAttribs = [|yield box <| CompilationMappingAttribute(SourceConstructFlags.SumType) |]
    
    let unionCases = ResizeArray<_>()
    
    do base.AddMember tagsType
      
    static let defaultAttributes isErased = 
         TypeAttributes.Public ||| TypeAttributes.Class ||| TypeAttributes.Sealed ||| enum (if isErased then int32 TypeProviderTypeAttributes.IsErased else 0)
                
    new (assembly:Assembly, namespaceName, className, baseType, ?hideObjectMethods, ?nonNullable, ?isErased) = 
        let isErased = defaultArg isErased true
        let nonNullable = defaultArg nonNullable false
        let hideObjectMethods = defaultArg hideObjectMethods false
        let attrs = defaultAttributes isErased
        //if not isErased && assembly.GetType().Name <> "ProvidedAssembly" then failwithf "a non-erased (i.e. generative) ProvidedTypeDefinition '%s.%s' was placed in an assembly '%s' that is not a ProvidedAssembly" namespaceName className (assembly.GetName().Name)
        ProvidedUnion(false, TypeContainer.Namespace (K assembly, namespaceName), className, K baseType, attrs, K None, [], None, None, K [| |], nonNullable, hideObjectMethods)

    new (className:string, baseType, ?hideObjectMethods, ?nonNullable, ?isErased) = 
        let isErased = defaultArg isErased true
        let nonNullable = defaultArg nonNullable false
        let hideObjectMethods = defaultArg hideObjectMethods false
        let attrs = defaultAttributes isErased
        ProvidedUnion(false, TypeContainer.TypeToBeDecided, className, K baseType, attrs, K None, [], None, None, K [| |], nonNullable, hideObjectMethods)
        
    override __.GetCustomAttributes(_inherit) = unionAttribs
    override __.GetCustomAttributes(_attributeType, _inherit) = unionAttribs
        
    //fields is set to just one for now
    member __.AddUnionCase(tag: int, position: int, name: string, [field]: PropertyInfo list) =

        //Add tag to the `Tags` nested class
        tagsType.AddMember(ProvidedField.Literal(name, typeof<int>, tag))
        
        //Add type and add a tag property to that to satisfy getUnionTypeTagNameMap in Union reflection
        let unionCaseType = ProvidedTypeDefinition(name, Some typeof<obj>, isErased = false)
        let unionTag = ProvidedProperty("Tag", typeof<int>, getterCode = fun _ -> Microsoft.FSharp.Quotations.Expr.Value tag)
        let fieldProp =
            let fp = field
            let fieldProp = ProvidedProperty("Item", fp.PropertyType, getterCode = fun _ -> Unchecked.defaultof<_>)
            //[CompilationMapping(SourceConstructFlags.Field, 0, 0)]
            let compilationAttributeType = typeof<CompilationMappingAttribute>
            let constructor = compilationAttributeType.TryGetConstructor([|typeof<SourceConstructFlags>; typeof<int>; typeof<int> |])
            let arguments = 
            // CompilationMappingAttribute(sourceConstruct, variantNo, sequenceNo)
            // type X = One of int * int //variant 0, sequenceNo 0,1 on Item1/Item2 properties
            // type X =
            //   | One of int // variant 0, sequenceNo 0 on Item prop
            //   | Two of int // variant 1, sequenceNo 0 on Item prop
                [| CustomAttributeTypedArgument (typeof<SourceConstructFlags>, SourceConstructFlags.Field)
                   CustomAttributeTypedArgument (typeof<int>, tag)
                   CustomAttributeTypedArgument (typeof<int>, 0)|]
            fieldProp.AddCustomAttribute(CustomAttributeData.Make(constructor.Value, args = arguments))
            fieldProp
            
        unionCaseType.AddMember unionTag
        unionCaseType.AddMember fieldProp
        __.AddMember unionCaseType           
        
        unionCases.Add { tag = tag
                         position = position
                         name = name
                         declaringType = __
                         fields = [field]
                         unionCaseType = unionCaseType }
        
    member __.UnionCases = unionCases.ToArray()

      
type ProvidedRecord(isTgt: bool, container:TypeContainer, className: string, getBaseType: (unit -> Type option), attrs: TypeAttributes, getEnumUnderlyingType, staticParams, staticParamsApply, backingDataSource, customAttributesData, nonNullable, hideObjectMethods) =
    inherit ProvidedTypeDefinition(isTgt, container, className, getBaseType, attrs,  getEnumUnderlyingType, staticParams, staticParamsApply, backingDataSource, customAttributesData, nonNullable, hideObjectMethods)
    static let defaultAttributes isErased = 
             TypeAttributes.Public ||| TypeAttributes.Class ||| TypeAttributes.Sealed ||| enum (if isErased then int32 TypeProviderTypeAttributes.IsErased else 0)

    new (assembly:Assembly, namespaceName, className, baseType, ?hideObjectMethods, ?nonNullable, ?isErased) = 
        let isErased = defaultArg isErased true
        let nonNullable = defaultArg nonNullable false
        let hideObjectMethods = defaultArg hideObjectMethods false
        let attrs = defaultAttributes isErased
        //if not isErased && assembly.GetType().Name <> "ProvidedAssembly" then failwithf "a non-erased (i.e. generative) ProvidedTypeDefinition '%s.%s' was placed in an assembly '%s' that is not a ProvidedAssembly" namespaceName className (assembly.GetName().Name)
        ProvidedRecord(false, TypeContainer.Namespace (K assembly, namespaceName), className, K baseType, attrs, K None, [], None, None, K [| |], nonNullable, hideObjectMethods)

    new (className:string, baseType, ?hideObjectMethods, ?nonNullable, ?isErased) = 
        let isErased = defaultArg isErased true
        let nonNullable = defaultArg nonNullable false
        let hideObjectMethods = defaultArg hideObjectMethods false
        let attrs = defaultAttributes isErased
        ProvidedRecord(false, TypeContainer.TypeToBeDecided, className, K baseType, attrs, K None, [], None, None, K [| |], nonNullable, hideObjectMethods)

module ProvidedUnion =
    let inline private apply (uc: ProvidedUnion) f = uc.UnionCases |> Seq.tryFind f
    let tryGetUnionCaseByTag tag (uc:ProvidedUnion) =
        apply uc (fun uc -> uc.tag = tag)
        
    let tryGetUnionCaseByName name (uc:ProvidedUnion) =
        apply uc (fun uc -> uc.name = name)
        
    let tryGetUnionCaseByPosition position (uc:ProvidedUnion) =
        apply uc (fun uc -> uc.position = position)