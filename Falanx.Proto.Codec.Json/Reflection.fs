[<RequireQualifiedAccess>]
module Falanx.Proto.Codec.Json.SafeReflection
open System
open System.Reflection

//thgis module is cloned from FSharp.Core, the only way to control some aspects of reflection on
// provided types safely is to tweak the implementation here.
module internal Impl =
    let inline checkNonNull argName (v: 'T) = 
        match box v with 
        | null -> nullArg argName 
        | _ -> ()
    
    let tryFindCompilationMappingAttribute (attrs:obj[]) =
      match attrs with
      | null | [| |] -> None
      | [| res |] -> let a = (res :?> CompilationMappingAttribute) in Some (a.SourceConstructFlags, a.SequenceNumber, a.VariantNumber)
      | _ -> failwith "multipleCompilationMappings"
          
    #if !FX_NO_REFLECTION_ONLY
    let cmaName = typeof<CompilationMappingAttribute>.FullName
    let assemblyName = typeof<CompilationMappingAttribute>.Assembly.GetName().Name 
    
    let tryFindCompilationMappingAttributeFromData (attrs:System.Collections.Generic.IList<CustomAttributeData>) =
        match attrs with
        | null -> None
        | _ -> 
            let mutable res = None
            for a in attrs do
                if a.Constructor.DeclaringType.FullName = cmaName then 
                    let args = a.ConstructorArguments
                    let flags = 
                         match args.Count  with 
                         | 1 -> ((let x = args.[0] in x.Value :?> SourceConstructFlags), 0, 0)
                         | 2 -> ((let x = args.[0] in x.Value :?> SourceConstructFlags), (let x = args.[1] in x.Value :?> int), 0)
                         | 3 -> ((let x = args.[0] in x.Value :?> SourceConstructFlags), (let x = args.[1] in x.Value :?> int), (let x = args.[2] in x.Value :?> int))
                         | _ -> (enum 0, 0, 0)
                    res <- Some flags
            res

    let findCompilationMappingAttributeFromData attrs =
      match tryFindCompilationMappingAttributeFromData attrs with
      | None -> failwith "no compilation mapping attribute"
      | Some a -> a
#endif 

    let tryFindCompilationMappingAttributeFromType       (typ:Type)        = 
//#if !FX_NO_REFLECTION_ONLY
        let assem = typ.Assembly
        if (not (isNull assem)) && assem.ReflectionOnly then 
           tryFindCompilationMappingAttributeFromData ( typ.GetCustomAttributesData())
        else None
//#endif
//        tryFindCompilationMappingAttribute ( typ.GetCustomAttributes (typeof<CompilationMappingAttribute>,false))
    
    let tryFindSourceConstructFlagsOfType (typ:Type) = 
        match tryFindCompilationMappingAttributeFromType typ with 
        | None -> None
        | Some (flags,_n,_vn) -> Some flags
    
    // Check the base type - if it is also an F# type then
    // for the moment we know it is a Discriminated Union
    let isExceptionRepr (typ:Type,bindingFlags) = 
        match tryFindSourceConstructFlagsOfType(typ) with 
        | None -> false 
        | Some(flags) -> 
          ((flags &&& SourceConstructFlags.KindMask) = SourceConstructFlags.Exception) &&
          // We see private representations only if BindingFlags.NonPublic is set
          (if (flags &&& SourceConstructFlags.NonPublicRepresentation) <> enum(0) then 
              (bindingFlags &&& BindingFlags.NonPublic) <> enum(0)
           else 
              true)
    let isNamedType(typ:Type) = not (typ.IsArray || typ.IsByRef || typ.IsPointer)
              
    let equivHeadTypes (ty1:Type) (ty2:Type) = 
        isNamedType(ty1) &&
        if ty1.IsGenericType then 
          ty2.IsGenericType && (ty1.GetGenericTypeDefinition()).Equals(ty2.GetGenericTypeDefinition())
        else 
          ty1.Equals(ty2)
                    
    let isOptionType typ = equivHeadTypes typ (typeof<int option>)
    let isFunctionType typ = equivHeadTypes typ (typeof<(int -> int)>)
    let isListType typ = equivHeadTypes typ (typeof<int list>)
    
    let isUnionType (typ:Type,bindingFlags:BindingFlags) = 
        isOptionType typ || 
        isListType typ || 
        match tryFindSourceConstructFlagsOfType(typ) with 
        | None -> false
        | Some(flags) ->
          (flags &&& SourceConstructFlags.KindMask) = SourceConstructFlags.SumType &&
          // We see private representations only if BindingFlags.NonPublic is set
          (if (flags &&& SourceConstructFlags.NonPublicRepresentation) <> enum(0) then 
              (bindingFlags &&& BindingFlags.NonPublic) <> enum(0)
           else 
              true)
              
    // Check the base type - if it is also an F# type then
    // for the moment we know it is a Discriminated Union
    let isConstructorRepr (typ:Type,bindingFlags:BindingFlags) = 
        let rec get (typ:Type) = isUnionType (typ,bindingFlags) || match typ.BaseType with null -> false | b -> get b
        get typ
        
        
    let unionTypeOfUnionCaseType (typ:Type,bindingFlags) = 
        let rec get (typ:Type) = if isUnionType (typ,bindingFlags) then typ else match typ.BaseType with null -> typ | b -> get b
        get typ
        
    let rec isClosureRepr typ = 
        isFunctionType typ || 
        (match typ.BaseType with null -> false | bty -> isClosureRepr bty) 
            
    let getTypeOfReprType (typ:Type,bindingFlags) = 
        if isExceptionRepr(typ,bindingFlags) then typ.BaseType
        elif isConstructorRepr(typ,bindingFlags) then unionTypeOfUnionCaseType(typ,bindingFlags)
        elif isClosureRepr(typ) then 
          let rec get (typ:Type) = if isFunctionType typ then typ else match typ.BaseType with null -> typ | b -> get b
          get typ 
        else typ
        
    let getFunctionTypeInfo (typ:Type) =
      if not (isFunctionType typ) then invalidArg "typ" (sprintf "%s is not a function type" typ.FullName)
      let tyargs = typ.GetGenericArguments()
      tyargs.[0], tyargs.[1]

type FSharpType =

    static member IsUnion(typ:Type,?bindingFlags) =  
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.checkNonNull "typ" typ
        let typ = Impl.getTypeOfReprType (typ ,BindingFlags.Public ||| BindingFlags.NonPublic)
        Impl.isUnionType (typ,bindingFlags)

    static member IsFunction(typ:Type) =  
        Impl.checkNonNull "typ" typ
        let typ = Impl.getTypeOfReprType (typ ,BindingFlags.Public ||| BindingFlags.NonPublic)
        Impl.isFunctionType typ
        
    static member GetFunctionElements(functionType:Type) =
        Impl.checkNonNull "functionType" functionType
        let functionType = Impl.getTypeOfReprType (functionType ,BindingFlags.Public ||| BindingFlags.NonPublic)
        Impl.getFunctionTypeInfo functionType

