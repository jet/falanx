module Falanx.Program

open Expecto
open System
open System.IO

[<EntryPoint>]
let main argv =
    JsonTests.jFormatterTests
    |> runTests defaultConfig
