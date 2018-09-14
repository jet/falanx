namespace Falanx.BinaryCodec
open System
open System.IO
open Froto.Serialization
open Froto.Serialization.Encoding.WireFormat
open System.Runtime.CompilerServices

[<RequireQualifiedAccess>]
module ZeroCopyBuffer =
    let allFields (zcb: ZeroCopyBuffer) =
        seq {
            // 7uy: each field is prefixed with its position (int) shiftet left by 3 bits,
            // so min value is 0000 1000 (in binary) = 8
            while (not zcb.IsEof) && zcb.Array.[int zcb.Position] > 7uy do
                yield Unpack.fromField zcb
        }   