namespace Falanx.Machinery
open System
open System.Reflection
open System.Collections.Generic

module Reflection =

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