module Falanx.Tests

open System
open System.IO
open Expecto
open Expecto.Logging
open Expecto.Logging.Message
open Fleece
open Newtonsoft

let jsonTests =
  testList "json" [
    testCase "package directive" (fun () ->
        let json = """{ "martId": 10, "memberId": "a" }"""
        match parseJson json with
        | Result.Ok (msg: Sample.Tests.WithPackage.BundleRequest) ->
            Expect.equal (Some(10)) msg.martId (sprintf "Failed to deserialize martId, msg was %A" msg)
            Expect.equal (Some "a") msg.memberId (sprintf "Failed to deserialize memberId, msg was %A" msg)
        | Result.Error err ->
            failwithf "Failed to parse json: %A" err
    )
  ]

let tests =
  testList "general" [
    jsonTests
  ]
