namespace Falanx.Machinery
open System
open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Falanx.Machinery.Reflection
      
type ProvidedUnionCase =
    { tag : int 
      name : string
      position : int
      declaringType : ProvidedUnion
      fields: PropertyInfo list
      unionCaseType: ProvidedTypeDefinition }  

and ProvidedUnion(isTgt: bool, container:TypeContainer, className: string, getBaseType: (unit -> Type option), attrs: TypeAttributes, getEnumUnderlyingType, staticParams, staticParamsApply, backingDataSource, customAttributesData, nonNullable, hideObjectMethods) =
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
        
    override this.GetCustomAttributes(_inherit) = unionAttribs
    override this.GetCustomAttributes(_attributeType, _inherit) =
        unionAttribs
        //// typeof(SomeType).IsAssignableFrom(typeof(Derived))
        //|> Array.filter (fun x -> x.GetType().IsAssignableFrom(_attributeType))
        
    //fields is set to just one for now
    member this.AddUnionCase(tag: int, position: int, name: string, fields: ProvidedProperty list) =
        let field = match fields with [field] -> field | _ -> failwith "Multiple fields are not yet supported as Union Cases"
        //Add tag to the `Tags` nested class
        tagsType.AddMember(ProvidedField.Literal(name, typeof<int>, tag))
        
        //Add type and add a tag property to that to satisfy getUnionTypeTagNameMap in Union reflection
        let unionCaseType = ProvidedTypeDefinition(name, Some typeof<obj>, isErased = false)
        let unionTag = ProvidedProperty("Tag", typeof<int>, getterCode = fun _ -> Microsoft.FSharp.Quotations.Expr.Value tag)
        
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
        field.AddCustomAttribute(CustomAttributeData.Make(constructor.Value, args = arguments))
            
        unionCaseType.AddMember unionTag
        unionCaseType.AddMember field
        this.AddMember unionCaseType           
        
        unionCases.Add { tag = tag
                         position = position
                         name = name
                         declaringType = this
                         fields = [field]
                         unionCaseType = unionCaseType }
        
    member __.UnionCases = unionCases.ToArray()

type ProvidedRecordProperty(isTgt: bool, propertyName: string, attrs: PropertyAttributes, propertyType: Type, isStatic: bool, getter: (unit -> MethodInfo) option, setter: (unit -> MethodInfo) option, indexParameters: ProvidedParameter[], customAttributesData) =
    inherit ProvidedProperty(isTgt, propertyName, attrs, propertyType, isStatic, getter, setter, indexParameters, customAttributesData)
    
    new (propertyName, propertyType, ?getterCode, ?setterCode, ?isStatic, ?indexParameters) =
            let isStatic = defaultArg isStatic false
            let indexParameters = defaultArg indexParameters []
            let pattrs = (if isStatic then MethodAttributes.Static else enum<MethodAttributes>(0)) ||| MethodAttributes.Public ||| MethodAttributes.SpecialName
            let getter = getterCode |> Option.map (fun _ -> ProvidedMethod(false, "get_" + propertyName, pattrs, Array.ofList indexParameters, propertyType, getterCode, [], None, K [| |]) :> MethodInfo)
            let setter = setterCode |> Option.map (fun _ -> ProvidedMethod(false, "set_" + propertyName, pattrs, [| yield! indexParameters; yield ProvidedParameter(false, "value",propertyType,isOut=Some false,optionalValue=None) |], typeof<Void>, setterCode, [], None, K [| |]) :> MethodInfo)
            ProvidedRecordProperty(false, propertyName, PropertyAttributes.None, propertyType, isStatic, Option.map K getter, Option.map K setter, Array.ofList indexParameters, K [| |])

type ProvidedRecord(isTgt: bool, container:TypeContainer, className: string, getBaseType: (unit -> Type option), attrs: TypeAttributes, getEnumUnderlyingType, staticParams, staticParamsApply, backingDataSource, customAttributesData, nonNullable, hideObjectMethods) =
    inherit ProvidedTypeDefinition(isTgt, container, className, getBaseType, attrs,  getEnumUnderlyingType, staticParams, staticParamsApply, backingDataSource, customAttributesData, nonNullable, hideObjectMethods)
    
    let recordAttribs = [|(CompilationMappingAttribute(SourceConstructFlags.RecordType) :> Attribute)|] |> box |> unbox<obj[]>
    
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
        
    override this.GetCustomAttributes(_inherit) = recordAttribs
    override this.GetCustomAttributes(_attributeType, _inherit) = recordAttribs
    
    member this.RecordFields =
        this.GetProperties()
        |> Array.choose (function :? ProvidedRecordProperty as prp -> Some (prp :> PropertyInfo) | _ -> None)
        |> Array.toList

module ProvidedUnion =
    let inline private tryApply (uc: ProvidedUnion) f = uc.UnionCases |> Seq.tryFind f
    let inline private apply (uc: ProvidedUnion) f = uc.UnionCases |> Seq.find f
    let tryGetUnionCaseByTag tag (uc:ProvidedUnion) =
        tryApply uc (fun uc -> uc.tag = tag)
        
    let tryGetUnionCaseByName (name : string) (uc:ProvidedUnion) =
        tryApply uc (fun uc -> uc.name = name)
        
    let tryGetUnionCaseByPosition position (uc:ProvidedUnion) =
        tryApply uc (fun uc -> uc.position = position)
        
    let getUnionCaseByTag tag (uc:ProvidedUnion) =
        apply uc (fun uc -> uc.tag = tag)
        
    let getUnionCaseByName (name : string) (uc:ProvidedUnion) =
        apply uc (fun uc -> uc.name = name)
        
    let getUnionCaseByPosition position (uc:ProvidedUnion) =
        apply uc (fun uc -> uc.position = position)