module Falanx.UnitTests

open System
open System.IO
open Expecto
open Expecto.Logging
open Expecto.Logging.Message

let generalTests =
  testList "general" [
    testCase "simple1" (fun () ->
      ()
    )
  ]

let tests () =
  [ generalTests ]
  |> testList "unit"
  |> testSequenced
