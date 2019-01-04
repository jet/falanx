namespace Falanx.Proto.Codec.Json
 
module ResizeArray =                         
    let flatten xs =
        match xs with
        | None -> ResizeArray()
        | Some x -> x
        
    let expand (x: ResizeArray<_>) =
        if isNull x || Seq.isEmpty x
        then None
        else Some x