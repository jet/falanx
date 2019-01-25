namespace Falanx.Machinery

open System
open System.Reflection
open ProviderImplementation.ProvidedTypes

type TypeHelpers() =

    static member FormatType(useFullName, ommitGenericArgs, t: Type, ?ommitEnclosingType, ?useQualifiedNames, ?knownNamespaces) =
        let ommitEnclosingType = defaultArg ommitEnclosingType None
        let knownNamespaces = defaultArg knownNamespaces Set.empty
        let useQualifiedNames = defaultArg useQualifiedNames false
                
        let fullName (t: Type) =
           let fullName =
               if useQualifiedNames && not (t :? ProvidedTypeDefinition) then
                   t.AssemblyQualifiedName
               else t.Namespace + "." + t.Name
           if fullName.StartsWith "FSI_" then
               fullName.Substring(fullName.IndexOf('.') + 1)
           else
               fullName
                        
        let hasUnitOfMeasure (t: System.Type) = 
            t.IsGenericType && 
            (t.Namespace = "System") &&
            (t.Name = typeof<bool>.Name ||
             t.Name = typeof<obj>.Name ||
             t.Name = typeof<int>.Name ||
             t.Name = typeof<int64>.Name ||
             t.Name = typeof<float>.Name ||
             t.Name = typeof<float32>.Name ||
             t.Name = typeof<decimal>.Name)
                                 
        let rec toString useFullName (ommitGenericArgs: bool) (t: Type) =
        
            let enclosingTypeSuppressed =
                match ommitEnclosingType with
                | Some (container:Type) when not (isNull t.DeclaringType) && container.Name = t.DeclaringType.Name -> true
                | _ -> false
                
            let hasUnit =  hasUnitOfMeasure t
        //                        | [ "System"; "Void"   ] -> ["unit"]
        //                        | [ "System"; "Char"   ] -> ["char"]
        //                        | [ "System"; "Byte"   ] -> ["byte"]
        //                        | [ "System"; "SByte"  ] -> ["sbyte"]
        //                        | [ "System"; "Int16"  ] -> ["int16"]
        //                        | [ "System"; "UInt16" ] -> ["uint16" ]
        //                        | [ "System"; "UInt32" ] -> ["uint32" ]
        //                        | [ "System"; "UInt64" ] -> ["uint64" ]
        //                        | [ "System"; "IntPtr" ] -> ["nativeint" ]
        //                        | [ "System"; "UIntPtr" ] -> ["unativeint" ]
        
            let rec innerToString (t: Type) =
                match t with
                | _ when t.Name = typeof<bool>.Name && not hasUnit -> "bool"
                | _ when t.Name = typeof<obj>.Name && not hasUnit  -> "obj"
                | _ when t.Name = typeof<int>.Name && not hasUnit  -> "int"
                | _ when t.Name = typeof<int64>.Name && not hasUnit  -> "int64"
                | _ when t.Name = typeof<float>.Name && not hasUnit  -> "float"
                | _ when t.Name = typeof<float32>.Name && not hasUnit  -> "float32"
                | _ when t.Name = typeof<decimal>.Name && not hasUnit  -> "decimal"
                | _ when t.Name = typeof<string>.Name && not hasUnit  -> "string"
                | _ when t.Name = typeof<Void>.Name -> "()"
                | _ when t.Name = typeof<unit>.Name -> "()"
                | t when t.IsArray -> (t.GetElementType() |> toString useFullName ommitGenericArgs) + "[]"
                | :? ProvidedTypeDefinition as t ->
                    //add t
                    t.Name.Split(',').[0]
                | t when t.IsGenericType ->
                    let args =
                        if useFullName then
                            t.GetGenericArguments()
                            |> Seq.map (if hasUnit then (fun t -> t.Name) else toString useFullName ommitGenericArgs)
                        else
                            t.GetGenericArguments()
                            |> Seq.map innerToString
                    if t.FullName.StartsWith "System.Tuple`" then
                        String.concat " * " args
                    elif t.Name.StartsWith "FSharpFunc`" then
                        "(" + (String.concat " -> " args) + ")"
                    else
                        let args = String.concat "," args
                        let name, reverse =
                            match t with
                            | t when hasUnit -> toString useFullName ommitGenericArgs t.UnderlyingSystemType, false
                            // Short names for some known generic types
                            | t when not useQualifiedNames && t.GetGenericTypeDefinition().Name = typeof<int seq>.GetGenericTypeDefinition().Name -> "seq", true
                            | t when not useQualifiedNames && t.GetGenericTypeDefinition().Name = typeof<int list>.GetGenericTypeDefinition().Name -> "list", true
                            | t when not useQualifiedNames && t.GetGenericTypeDefinition().Name = typeof<int option>.GetGenericTypeDefinition().Name -> "option", true
                            | t when not useQualifiedNames && t.GetGenericTypeDefinition().Name = typeof<int ref>.GetGenericTypeDefinition().Name -> "ref", true
                            | t when not useQualifiedNames && t.GetGenericTypeDefinition().Name = typeof<int ResizeArray>.GetGenericTypeDefinition().Name -> "ResizeArray", true
                            | t when not useQualifiedNames && t.GetGenericTypeDefinition().Name = typeof<Result<int,int>>.GetGenericTypeDefinition().Name -> "Result", true
                            | t when not useQualifiedNames && t.Name = "FSharpAsync`1" -> "async", true
                            // Short names for types in F# namespaces
                            | t when not useQualifiedNames && knownNamespaces.Contains t.Namespace -> t.Name, false
                            | t -> (if useFullName then fullName t else t.Name), false
                        let name = name.Split('`').[0]
                        if ommitGenericArgs then name
                        else
                            if reverse then
                                args + " " + name
                            else
                                name + "<" + args + ">"
                // Short names for types in F# namespaces
                | t when not useQualifiedNames && knownNamespaces.Contains t.Namespace -> t.Name
                // Short names for generic parameters
                | t when t.IsGenericParameter -> t.Name
                | t -> if useFullName then fullName t else t.Name
        
            let rec warnIfWrongAssembly (t:Type) =
                match t with
                | t when t.GetType().Name = "ProvidedTypeDefinition" -> ""
                | t when t.IsGenericType -> defaultArg (t.GetGenericArguments() |> Seq.map warnIfWrongAssembly |> Seq.tryFind (fun s -> s <> "")) ""
                | t when t.IsArray -> warnIfWrongAssembly <| t.GetElementType()
                | t -> if not t.IsGenericParameter && t.Assembly = Assembly.GetExecutingAssembly() then " [DESIGNTIME]" else ""
        
            if hasUnit || t.IsGenericParameter || isNull t.DeclaringType || enclosingTypeSuppressed then 
                innerToString t + (warnIfWrongAssembly t)
            else
                (toString useFullName ommitGenericArgs t.DeclaringType) + "." + (innerToString t) + (warnIfWrongAssembly t)
                
        let name = toString useFullName ommitGenericArgs t
        name