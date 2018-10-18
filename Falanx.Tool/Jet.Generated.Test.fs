namespace rec Jet.Generated

open System
open Froto.Serialization
open System.Collections.Generic
open Falanx.BinaryCodec
open Falanx.BinaryCodec.Primitives

[<CLIMutable>]
type BundleRequest =
    { mutable martId : int option
      mutable memberId : string option
      mutable channelType : string option
      mutable retailSkus : string ResizeArray }
    
    static member Serialize(m : BundleRequest, buffer : ZeroCopyBuffer) =
        Primitives.writeOption<Int32> (Primitives.writeInt32) (1u) (buffer) (m.martId)
        Primitives.writeOption<String> (Primitives.writeString) (2u) (buffer) (m.memberId)
        Primitives.writeOption<String> (Primitives.writeString) (3u) (buffer) (m.channelType)
        Primitives.writeRepeated<String> (Primitives.writeString) (4u) (buffer) (m.retailSkus)
    
    static member Deserialize(buffer : ZeroCopyBuffer) = Primitives.deserialize<BundleRequest> (buffer)
    interface IMessage with
        member x.Serialize(buffer : ZeroCopyBuffer) = BundleRequest.Serialize(x, buffer)
        
        member x.ReadFrom(buffer : ZeroCopyBuffer) =
            let enumerator : IEnumerator<Froto.Serialization.Encoding.RawField> =
                ZeroCopyBuffer.allFields(buffer).GetEnumerator()
            while enumerator.MoveNext() do
                let current : Froto.Serialization.Encoding.RawField = enumerator.Current
                if current.FieldNum = 4 then 
                    if Operators.isNull<List<String>> (x.retailSkus) then x.retailSkus <- new List<String>()
                    else ()
                    x.retailSkus.Add(Primitives.readString current)
                else if current.FieldNum = 3 then x.channelType <- Some(Primitives.readString current)
                else if current.FieldNum = 2 then x.memberId <- Some(Primitives.readString current)
                else if current.FieldNum = 1 then x.martId <- Some(Primitives.readInt32 current)
                else ()
            enumerator.Dispose()
        
        member x.SerializedLength() = Primitives.serializedLength<BundleRequest> (x)
