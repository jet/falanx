namespace Falanx.Machinery
open System
open System.Reflection
open System.Collections.Generic

module Reflection =

//thgis module is cloned from FSharp.Core, the only way to control some aspects of reflection on
// provided types safely is to tweak the implementation here.
    module internal Impl =
        let instancePropertyFlags = BindingFlags.Instance
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
            
        let maxTuple = 8
        
        // Which field holds the nested tuple?
        let tupleEncField = maxTuple - 1
        
        let simpleTupleNames = [|
            "Tuple`1";      "Tuple`2";      "Tuple`3";
            "Tuple`4";      "Tuple`5";      "Tuple`6";
            "Tuple`7";      "Tuple`8";      
            "ValueTuple`1"; "ValueTuple`2"; "ValueTuple`3";
            "ValueTuple`4"; "ValueTuple`5"; "ValueTuple`6";
            "ValueTuple`7"; "ValueTuple`8"; |]
            
        let isTupleType (typ:Type) = 
            // We need to be careful that we only rely typ.IsGenericType, typ.Namespace and typ.Name here.
            //
            // Historically the FSharp.Core reflection utilities get used on implementations of 
            // System.Type that don't have functionality such as .IsEnum and .FullName fully implemented.
            // This happens particularly over TypeBuilderInstantiation types in the ProvideTypes implementation of System.TYpe
            // used in F# type providers.
            typ.IsGenericType &&
            typ.Namespace = "System" && 
            simpleTupleNames |> Seq.exists typ.Name.StartsWith
            
        let orderTupleProperties (props:PropertyInfo[]) =
            // The tuple properties are of the form:
            //   Item1
            //   ..
            //   Item1, Item2, ..., Item<maxTuple-1>
            //   Item1, Item2, ..., Item<maxTuple-1>, Rest
            // The PropertyInfo may not come back in order, so ensure ordering here.
    #if !NETSTANDARD1_6
            assert(maxTuple < 10) // Alphasort will only works for upto 9 items: Item1, Item10, Item2, Item3, ..., Item9, Rest
    #endif
            let props = props |> Array.sortBy (fun p -> p.Name) // they are not always in alphabetic order
    #if !NETSTANDARD1_6  
            assert(props.Length <= maxTuple)
            assert(let haveNames   = props |> Array.map (fun p -> p.Name)
                   let expectNames = Array.init props.Length (fun i -> let j = i+1 // index j = 1,2,..,props.Length <= maxTuple
                                                                       if   j<maxTuple then "Item" + string j
                                                                       elif j=maxTuple then "Rest"
                                                                       else (assert false; "")) // dead code under prior assert, props.Length <= maxTuple
                   haveNames = expectNames)
    #endif
            props
            
        let getTupleReaderInfo (typ:Type,index:int) =
            if index < 0 then invalidArg "index" (sprintf "tuple: %s Index: %i Out Of Range" typ.FullName index)
    
            let get index =
                if typ.IsValueType then
                    let props = typ.GetProperties(instancePropertyFlags ||| BindingFlags.Public) |> orderTupleProperties
                    if index >= props.Length then invalidArg "index" (sprintf "tuple: %s Index: %i Out Of Range" typ.FullName index)
                    props.[index]
                else
                    let typ =
                        match typ.GetType() with
                        | t when t.Name.Contains "TypeBuilderInstantiation" ->
                            typ.GetGenericTypeDefinition()
                        | _ -> typ
                    let props = typ.GetProperties(instancePropertyFlags ||| BindingFlags.Public) |> orderTupleProperties
                    if index >= props.Length then invalidArg "index" (sprintf "tuple: %s Index: %i Out Of Range" typ.FullName index)
                    props.[index]
    
            if index < tupleEncField then
                get index, None
            else
                let etys = typ.GetGenericArguments()
                get tupleEncField, Some(etys.[tupleEncField],index-(maxTuple-1))
            
        let getFunctionTypeInfo (typ:Type) =
          if not (isFunctionType typ) then invalidArg "typ" (sprintf "%s is not a function type" typ.FullName)
          let tyargs = typ.GetGenericArguments()
          tyargs.[0], tyargs.[1]
          
        let checkTupleType(argName,(tupleType:Type)) =
            checkNonNull argName tupleType;
            if not (isTupleType tupleType) then invalidArg argName (sprintf "%s is not a tuple type" tupleType.FullName)


    type BindingFlags with
        static member AllInstance = BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.NonPublic     
            
    type CustomAttributeData with
        static member Make(ctorInfo, ?args, ?namedArgs) = 
            { new CustomAttributeData() with 
                member __.Constructor =  ctorInfo
                member __.ConstructorArguments = defaultArg args [||] :> IList<_>
                member __.NamedArguments = defaultArg namedArgs [||] :> IList<_> }
                    
    type Type with
        member x.GetConstructor(typ) =
            x.GetConstructor(BindingFlags.AllInstance, Type.DefaultBinder, [|typ|], [||] )
            
        member x.GetConstructor(typ: Type array) =
            x.GetConstructor(BindingFlags.AllInstance, Type.DefaultBinder, typ, [||] )
    
        member x.TryGetConstructor(typ:Type) =
            match x.GetConstructor(typ) with
            | null -> None
            | v -> Some v
    
        member x.TryGetConstructor(typ: Type array) =
            match x.GetConstructor(BindingFlags.AllInstance, Type.DefaultBinder, typ, [||] ) with
            | null -> None
            | v -> Some v
    
        member x.GetUnitConstructor() =
            x.GetConstructor([||])
    
        member x.TryGetUnitConstructor() =
            match x.GetUnitConstructor() with
            | null -> None
            | v -> Some v
    
        member x.GetVirtualMethods() = 
            x.GetMethods (BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly) 
            |> Seq.filter (fun m -> m.IsVirtual)
            


    type FSharpTypeSafe =

        static member IsUnion(typ: Type, ?bindingFlags) =  
            let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
            Impl.checkNonNull "typ" typ
            let typ = Impl.getTypeOfReprType (typ ,BindingFlags.Public ||| BindingFlags.NonPublic)
            Impl.isUnionType (typ,bindingFlags)
    
        static member IsFunction(typ: Type) =  
            Impl.checkNonNull "typ" typ
            let typ = Impl.getTypeOfReprType (typ ,BindingFlags.Public ||| BindingFlags.NonPublic)
            Impl.isFunctionType typ
            
        static member GetFunctionElements(functionType: Type) =
            Impl.checkNonNull "functionType" functionType
            let functionType = Impl.getTypeOfReprType (functionType ,BindingFlags.Public ||| BindingFlags.NonPublic)
            Impl.getFunctionTypeInfo functionType
            
    type FSharpValueSafe =
        
        static member PreComputeTuplePropertyInfo(tupleType:Type,index:int) =
            Impl.checkTupleType("tupleType",tupleType) 
            Impl.getTupleReaderInfo (tupleType,index)