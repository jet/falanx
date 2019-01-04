namespace PtAst

type SynType with
    static member CreateFromType(typ: Type) =             
        if typ.IsGenericType then
            let genericParams =
                typ.GetGenericArguments()
                |> Seq.map (fun gt -> TypeHelpers.toString false false gt
                                      |> LongIdentWithDots.CreateString
                                      |> SynType.CreateLongIdent ) |> Seq.toList
                
            let genericType =
                TypeHelpers.toString false true (typ.GetGenericTypeDefinition())
                |> LongIdentWithDots.CreateString
                |> SynType.CreateLongIdent
            
            SynType.CreateApp(genericType, genericParams, true)
            
        else
            SynType.CreateLongIdent(LongIdentWithDots.CreateString (typ.ToString()) ) 
