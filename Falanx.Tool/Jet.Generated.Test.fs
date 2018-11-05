namespace rec Jet.Generated

open System
open Froto.Serialization
open System.Collections.Generic
open Falanx.Proto.Codec.Binary
open Falanx.Proto.Codec.Binary.Primitives

module SampleMessage =
    type test_oneof =
        | First_name of First_name : string
        | Age of Age : int
        | Last_name of Last_name : string

[<CLIMutable>]
type SampleMessage =
    { mutable testOneof : SampleMessage.test_oneof option
      mutable martId : int option }
    
    static member Serialize(m : SampleMessage, buffer : ZeroCopyBuffer) =
        Primitives.writeOption<Int32> (Primitives.writeInt32) (1) (buffer) (m.martId)
        if match m.testOneof with
           | Some _ -> true
           | _ -> false
        then 
            if match Option.get<SampleMessage.test_oneof> (m.testOneof) with
               | SampleMessage.Last_name _ -> true
               | _ -> false
            then Primitives.writeString 7 buffer (match Option.get<SampleMessage.test_oneof> (m.testOneof) with
                                                  | SampleMessage.Last_name(Last_name = x) -> x
                                                  | _ -> failwith "Should never hit")
            else if match Option.get<SampleMessage.test_oneof> (m.testOneof) with
                    | SampleMessage.Age _ -> true
                    | _ -> false
            then Primitives.writeInt32 9 buffer (match Option.get<SampleMessage.test_oneof> (m.testOneof) with
                                                 | SampleMessage.Age(Age = x) -> x
                                                 | _ -> failwith "Should never hit")
            else if match Option.get<SampleMessage.test_oneof> (m.testOneof) with
                    | SampleMessage.First_name _ -> true
                    | _ -> false
            then Primitives.writeString 4 buffer (match Option.get<SampleMessage.test_oneof> (m.testOneof) with
                                                  | SampleMessage.First_name(First_name = x) -> x
                                                  | _ -> failwith "Should never hit")
            else ()
        else ()
    
    static member Deserialize(buffer : ZeroCopyBuffer) = Primitives.deserialize<SampleMessage> (buffer)
    interface IMessage with
        member x.Serialize(buffer : ZeroCopyBuffer) = SampleMessage.Serialize(x, buffer)
        
        member x.ReadFrom(buffer : ZeroCopyBuffer) =
            let enumerator : IEnumerator<Froto.Serialization.Encoding.RawField> =
                ZeroCopyBuffer.allFields(buffer).GetEnumerator()
            while enumerator.MoveNext() do
                let current : Froto.Serialization.Encoding.RawField = enumerator.Current
                if current.FieldNum = 7 then x.testOneof <- Some(SampleMessage.Last_name(Primitives.readString current))
                else if current.FieldNum = 9 then x.testOneof <- Some(SampleMessage.Age(Primitives.readInt32 current))
                else if current.FieldNum = 4 then 
                    x.testOneof <- Some(SampleMessage.First_name(Primitives.readString current))
                else if current.FieldNum = 1 then x.martId <- Some(Primitives.readInt32 current)
                else ()
            enumerator.Dispose()
        
        member x.SerializedLength() = Primitives.serializedLength<SampleMessage> (x)
