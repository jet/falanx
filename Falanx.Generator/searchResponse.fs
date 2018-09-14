namespace Falanx.Generated

open System
open System.Collections.Generic
open Falanx.BinaryCodec
open Falanx.BinaryCodec.Primitives

[<CLIMutable>]
type Result =
    { mutable url : string option
      mutable title : string option
      mutable snippets : string ResizeArray }
    
    static member Serialize(m : Result, buffer : ZeroCopyBuffer) =
        Primitives.writeOption<String> (Primitives.writeString) (1) (buffer) (m.url)
        Primitives.writeOption<String> (Primitives.writeString) (2) (buffer) (m.title)
        Primitives.writeRepeated<String> (Primitives.writeString) (3) (buffer) (m.snippets)
    
    interface IMessage with
        member x.Serialize(buffer : ZeroCopyBuffer) = Result.Serialize(x, buffer)
        
        member x.ReadFrom(buffer : ZeroCopyBuffer) =
            let enumerator : IEnumerator<Froto.Serialization.Encoding.RawField> =
                ZeroCopyBuffer.allFields(buffer).GetEnumerator()
            while enumerator.MoveNext() do
                let current : Froto.Serialization.Encoding.RawField = enumerator.Current
                if current.FieldNum = 3 then 
                    if ProviderImplementation.ProvidedTypes.Utils.isNull<List<String>> (x.snippets) then 
                        x.snippets <- new List<String>()
                    else ()
                    x.snippets.Add(Primitives.readString current)
                else if current.FieldNum = 2 then x.title <- Some(Primitives.readString current)
                else if current.FieldNum = 1 then x.url <- Some(Primitives.readString current)
                else ()
            enumerator.Dispose()
        
        member x.SerializedLength() = Primitives.serializedLength<Result> (x)

[<CLIMutable>]
type SearchResponse =
    { mutable results : Result ResizeArray }
    static member Serialize(m : SearchResponse, buffer : ZeroCopyBuffer) =
        Primitives.writeRepeatedEmbedded<Result> (1, buffer, m.results)
    interface IMessage with
        member x.Serialize(buffer : ZeroCopyBuffer) = SearchResponse.Serialize(x, buffer)
        
        member x.ReadFrom(buffer : ZeroCopyBuffer) =
            let enumerator : IEnumerator<Froto.Serialization.Encoding.RawField> =
                ZeroCopyBuffer.allFields(buffer).GetEnumerator()
            while enumerator.MoveNext() do
                let current : Froto.Serialization.Encoding.RawField = enumerator.Current
                if current.FieldNum = 1 then 
                    if ProviderImplementation.ProvidedTypes.Utils.isNull<List<Result>> (x.results) then 
                        x.results <- new List<Result>()
                    else ()
                    x.results.Add(Primitives.readEmbedded<Result> (current))
                else ()
            enumerator.Dispose()
        
        member x.SerializedLength() = Primitives.serializedLength<SearchResponse> (x)
