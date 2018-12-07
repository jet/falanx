namespace Falanx.Proto.Codec.Binary
    open System
    open Froto.Serialization
    open Froto.Serialization.Encoding
    open Froto.Serialization.Encoding.WireFormat
    
    // type aliases based on https://developers.google.com/protocol-buffers/docs/proto3#scalar
    type proto_double = float
    type proto_float = float32
    type proto_int32 = int
    type proto_int64 = int64
    type proto_uint32 = uint32
    type proto_uint64 = uint64
    type proto_sint32 = int
    type proto_sint64 = int64
    type proto_fixed32 = uint32
    type proto_fixed64 = uint64
    type proto_sfixed32 = int
    type proto_sfixed64 = int64
    type proto_bool = bool
    type proto_string = string
    type proto_bytes = ArraySegment<byte>
    type proto_repeated<'T> = ResizeArray<'T>
    type proto_map<'Key, 'Value> = Collections.Generic.Dictionary<'Key, 'Value>   
          
    type Writer<'T> = FieldNum -> ZeroCopyBuffer -> 'T -> unit
    type Reader<'T> = RawField -> 'T

    // Eventually this interface could be addd to Froto itself
    /// Interface for types generated from proto messages.
    type IMessage =
        abstract member SerializedLength: unit -> uint32
        abstract member Serialize: ZeroCopyBuffer -> unit
        abstract member ReadFrom: ZeroCopyBuffer -> unit
    
    /// Simple implementation of Message class that does nothing useful
    /// Basically, this class is needed only for type inference within quotations, because it satisfies requirements
    /// to be inherited from Message and to have constructor without parameters
    type Template() = 
        interface IMessage with
            member x.Serialize(_) = ()
            member x.ReadFrom(_) = ()
            member x.SerializedLength() = 0u
        
    /// Simple type used to simplify maps serialization and deserialization
    type internal MapItem<'Key, 'Value>
        ( keyReader: Reader<'Key>,
          valueReader: Reader<'Value>,
          keyWriter: Writer<'Key>,
          valueWriter: Writer<'Value> ) =
                
        member val Key = Unchecked.defaultof<_> with get, set
        member val Value = Unchecked.defaultof<_> with get, set
        
        interface IMessage with
            member x.Serialize(buffer) =
                keyWriter 1 buffer x.Key
                valueWriter 2 buffer x.Value
            
            member x.ReadFrom(buffer) =
                for field in ZeroCopyBuffer.allFields buffer do
                    if field.FieldNum = 1 then
                        x.Key <- keyReader field
                    elif field.FieldNum = 2 then
                        x.Value <- valueReader field
                        
            member x.SerializedLength() =
                let buffer = NullWriteBuffer()
                (x :> IMessage).Serialize buffer
                buffer.Length
                
    module Primitives =
           
        let write f (fieldNumber: FieldNum) (buffer: ZeroCopyBuffer) value =
            f fieldNumber value buffer |> ignore
        
        let writeDouble: Writer<proto_double> = write Encode.fromDouble
        let writeFloat: Writer<proto_float> = write Encode.fromSingle
        let writeInt32: Writer<proto_int32> = write Encode.fromVarint
        let writeInt64: Writer<proto_int64> = write Encode.fromVarint
        let writeUInt32: Writer<proto_uint32> = write Encode.fromVarint
        let writeUInt64: Writer<proto_uint64> = write Encode.fromVarint
        let writeSInt32: Writer<proto_sint32> = write Encode.fromSInt32
        let writeSInt64: Writer<proto_sint64> = write Encode.fromSInt64
        let writeFixed32: Writer<proto_fixed32> = write Encode.fromFixed32
        let writeFixed64: Writer<proto_fixed64> = write Encode.fromFixed64
        let writeSFixed32: Writer<proto_sfixed32> = write Encode.fromSFixed32
        let writeSFixed64: Writer<proto_sfixed64> = write Encode.fromSFixed64
        let writeBool: Writer<proto_bool> = write Encode.fromBool
        let writeString: Writer<proto_string> = write Encode.fromString
        let writeBytes: Writer<proto_bytes> = write Encode.fromBytes
        
        /// Serializes optional field using provided function to handle inner value if present
        let writeOptional (writeInner: Writer<'T>) (position: FieldNum) buffer value =
            match value with
            | Some(v) -> writeInner position buffer v
            | None -> ()
            
        let writeOption (writer: Writer<'a>) (position: FieldNum) buffer value =
            Option.iter (writer (position: FieldNum) (buffer: ZeroCopyBuffer)) value
        
        
        let writeEmbedded position buffer (value: IMessage) =
            if value |> box |> isNull |> not then
                buffer
                |> Pack.toTag position WireType.LengthDelimited
                |> Pack.toVarint (uint64 <| value.SerializedLength() )
                |> value.Serialize
                
        let writeOptionalEmbedded position buffer value =
            value |> Option.iter (writeEmbedded position buffer)

        let writeUnionOptionalEmbedded unionType unionMembers =
            fun buffer value cases ->
                match value with
                | Some union ->
                    cases
                | _ -> ()
                
        let writeRepeated (writeItem: Writer<'T>) position buffer value =
            value |> Seq.iter (writeItem position buffer)
            
        let writeRepeatedEmbedded<'a  when 'a :> IMessage > (position, buffer, value ) =             
             for (v: 'a) in (value: ResizeArray<'a>) do
                    buffer
                    |> Pack.toTag position WireType.LengthDelimited
                    |> Pack.toVarint (uint64 <| (v).SerializedLength() )
                    |> v.Serialize  
                
        let private writeMap writeKey writeValue convertValue : Writer<proto_map<_, _>> =
            fun position buffer value ->
                let item = new MapItem<_, _>(Unchecked.defaultof<_>, Unchecked.defaultof<_>, writeKey, writeValue)
                for pair in value do
                    item.Key <- pair.Key
                    item.Value <- convertValue pair.Value
                    writeEmbedded position buffer item
        
        let writePrimitiveMap writeKey writeValue : Writer<proto_map<'Key, 'Value>> =
            fun position buffer value -> writeMap writeKey writeValue id position buffer value
            
        let writeMessageMap<'Key, 'Value when 'Value :> IMessage> writeKey =
            fun position buffer (value: proto_map<'Key, 'Value>) ->
                writeMap
                    writeKey
                    writeEmbedded
                    id
                    position
                    buffer
                    value
        
        let writeEnumMap<'Key> writeKey : Writer<proto_map<'Key, proto_int32>> =
            fun position buffer value ->
                writeMap writeKey writeInt32 id position buffer value
        
        let deserialize<'T when 'T :> IMessage and 'T : (new: unit -> 'T)> buffer =
            let x = new 'T()
            x.ReadFrom buffer
            x
            
        let serializedLength<'a when 'a :> IMessage> (i: 'a) =
            let buffer = NullWriteBuffer()
            i.Serialize buffer
            buffer.Length
                    
        let readDouble: Reader<proto_double> = Decode.toDouble
        let readFloat: Reader<proto_float> = Decode.toSingle
        let readInt32: Reader<proto_int32> = Decode.toInt32
        let readInt64: Reader<proto_int64> = Decode.toInt64
        let readUInt32: Reader<proto_uint32> = Decode.toUInt32
        let readUInt64: Reader<proto_uint64> = Decode.toUInt64
        let readSInt32: Reader<proto_sint32> = Decode.toSInt32
        let readSInt64: Reader<proto_sint64> = Decode.toSInt64
        let readFixed32: Reader<proto_fixed32> = Decode.toFixed32
        let readFixed64: Reader<proto_fixed64> = Decode.toFixed64
        let readSFixed32: Reader<proto_sfixed32> = Decode.toSFixed32
        let readSFixed64: Reader<proto_sfixed64> = Decode.toSFixed64
        let readBool: Reader<proto_bool> = Decode.toBool
        let readString: Reader<proto_string> = Decode.toString
        let readBytes: Reader<proto_bytes> = Decode.toBytes >> proto_bytes
        
        let readEmbedded<'T when 'T :> IMessage and 'T : (new: unit -> 'T)> field =
            match field with
            | LengthDelimited(_, segment) -> ZeroCopyBuffer segment |> deserialize<'T>
            | _ -> failwithf "Invalid format of the field: %O" field
        
        let readMapElement<'Key, 'Value> (map: proto_map<'Key, 'Value>) keyReader (valueReader: Reader<'Value>) field =
            match field with
            | LengthDelimited(_, segment) ->
                let item = MapItem(keyReader, valueReader, Unchecked.defaultof<_>, Unchecked.defaultof<_>) 
                (item :> IMessage).ReadFrom (ZeroCopyBuffer segment)
                map.Add(item.Key, item.Value)
            | _ -> failwithf "Invalid format of the field: %O" field
        
        let readMessageMapElement<'Key, 'Value when 'Value :> IMessage and 'Value : (new: unit -> 'Value)> (map: proto_map<'Key, 'Value>) keyReader field =
            readMapElement map keyReader readEmbedded<'Value> field
