module Falanx.UnitTests

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
        let json = """{ "single_int32": 10 }"""
        match parseJson json with
        | Result.Ok msg ->
            printfn "%A" msg
            Expect.equal (Some(10)) msg.singleInt32 "Failed to deserialize singleInt32"
            Expect.equal (Some(0L)) msg.singleInt64 "Expected default 0 on singleInt64"
            Expect.equal (Some("")) msg.singleString "Expected default empty string"
        | Result.Error err ->
            failwithf "Failed to parse json: %A" err
    )
  ]

let tests () =
  [ jsonTests ]
  |> testList "unit"
  |> testSequenced
