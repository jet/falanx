module GoogleProtobufTest.V3.CompatibilityTests

open System
open System.IO
open Expecto
open Expecto.Logging
open Expecto.Logging.Message
open Fleece
open Newtonsoft
open protobuf_unittest3

let jsonTests =
  testList "json" [
    testCase "Original field name accepted" (fun () ->
        let json = """{ "singleInt32": 10 }"""
        match parseJson json with
        | Result.Ok msg ->
            Expect.equal (Some(10)) msg.singleInt32 (sprintf "Failed to deserialize singleInt32, msg was %A" msg)
            Expect.equal None msg.singleInt64 (sprintf "Expected default 0 on singleInt64, msg was %A" msg)
            Expect.equal None msg.singleString (sprintf "Expected default empty string, msg was %A" msg)
        | Result.Error err ->
            failwithf "Failed to parse json: %A" err
    )
  ]

let compatibilityTests =
  testList "compatibility" [
    jsonTests
  ]
