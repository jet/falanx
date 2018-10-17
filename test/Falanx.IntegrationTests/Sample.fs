module Falanx.Tests

open System
open System.IO
open Expecto
open Expecto.Logging
open Expecto.Logging.Message
open FileUtils
open Medallion.Shell
open System.IO.Compression
open System.Xml.Linq
open Falanx.TestAssets

let RepoDir = (__SOURCE_DIRECTORY__ /".." /"..") |> Path.GetFullPath
let ExamplesDir = RepoDir/"test"/"examples"
let TestRunDir = RepoDir/"test"/"testrun"
let TestRunPackagesDir = TestRunDir/"packages"
let TestRunDirToolDir = TestRunDir/"tool"
let NupkgsDir = RepoDir/"artifact"/"nupkg"
let NugetOrgV3Url = "https://api.nuget.org/v3/index.json"

let checkExitCodeZero (cmd: Command) =
    Expect.equal 0 cmd.Result.ExitCode "command finished with exit code non-zero."

let dotnet (fs: FileUtils) args =
    fs.shellExecRun "dotnet" args

let renderNugetConfig clear feeds =
    [ yield "<configuration>"
      yield "  <packageSources>"
      if clear then
        yield "    <clear />"
      for (name, url) in feeds do
        yield sprintf """    <add key="%s" value="%s" />""" name url
      yield "  </packageSources>"
      yield "</configuration>" ]

let prepareTool (fs: FileUtils) pkgUnderTestVersion =
    fs.rm_rf TestRunDirToolDir
    fs.mkdir_p TestRunDirToolDir

    fs.mkdir_p (TestRunDirToolDir/"nuget")

    renderNugetConfig true ["local", NupkgsDir]
    |> writeLines
    |> fs.createFile (TestRunDirToolDir/"nuget"/"nuget.config") 

    fs.createFile (TestRunDirToolDir/"Directory.Build.props") (writeLines 
      [ """<Project ToolsVersion="15.0">"""
        "  <PropertyGroup>"
        "  </PropertyGroup>"
        "</Project>" ])

    fs.mkdir_p (TestRunDirToolDir/"bin")
    dotnet fs ["tool"; "install"; "Falanx.Tool"; "--version"; pkgUnderTestVersion; "--tool-path"; (TestRunDirToolDir/"bin"); "--configfile"; (TestRunDirToolDir/"nuget"/"nuget.config") ]
    |> checkExitCodeZero

let falanx (fs: FileUtils) args =
    fs.shellExecRun (TestRunDirToolDir/"bin"/"falanx") args

let copyDirFromAssets (fs: FileUtils) source outDir =
    fs.mkdir_p outDir

    let path = ExamplesDir/source

    fs.cp_r path outDir

let tests pkgUnderTestVersion =
 
  let prepareTestsAssets = lazy(
      let logger = Log.create "Tests Assets"
      let fs = FileUtils(logger)

      fs.rm_rf TestRunPackagesDir

      // restore tool
      prepareTool fs pkgUnderTestVersion

      fs.mkdir_p (TestRunDir/"tests")

      renderNugetConfig true ["nugetorg", NugetOrgV3Url; "local", NupkgsDir]
      |> writeLines
      |> fs.createFile (TestRunDir/"tests"/"nuget.config") 

      fs.createFile (TestRunDir/"tests"/"Directory.Build.props") (writeLines 
        [ """<Project ToolsVersion="15.0">"""
          "  <PropertyGroup>"
          sprintf "    <FalanxVersion>%s</FalanxVersion>" pkgUnderTestVersion
          sprintf "    <RestorePackagesPath>%s</RestorePackagesPath>" TestRunPackagesDir
          "  </PropertyGroup>"
          "</Project>" ])
    )

  let withLog name f test =
    test name (fun () ->
      prepareTestsAssets.Force()

      let logger = Log.create (sprintf "Test '%s'" name)
      let fs = FileUtils(logger)
      f logger fs)

  let withLogAsync name f test =
    test name (async {
      prepareTestsAssets.Force()

      let logger = Log.create (sprintf "Test '%s'" name)
      let fs = FileUtils(logger)
      do! f logger fs })

  let inDir (fs: FileUtils) dirName =
    let outDir = TestRunDir/"tests"/dirName
    fs.rm_rf outDir
    fs.mkdir_p outDir
    fs.cd outDir
    outDir

  let asLines (s: string) =
    s.Split(Environment.NewLine) |> List.ofArray

  let stdOutLines (cmd: Command) =
    cmd.Result.StandardOutput
    |> fun s -> s.Trim()
    |> asLines

  let generalTests =
    testList "general" [
      testCase |> withLog "can show help" (fun _ fs ->

        falanx fs ["--help"]
        |> checkExitCodeZero

      )
    ]

  let sanityChecks =
    testList "sanity check of projects" [

      testCase |> withLog "can build sample2" (fun _ fs ->
        let testDir = inDir fs "sanity_check_sample2"
        copyDirFromAssets fs ``samples2 library``.ProjDir testDir

        let projPath = testDir/ (``samples2 library``.ProjectFile)
        let projDir = Path.GetDirectoryName projPath

        fs.cd testDir
        dotnet fs ["build"; projPath]
        |> checkExitCodeZero

        let outputPath = projDir/"bin"/"Debug"/"netcoreapp2.1"/``samples2 library``.AssemblyName + ".dll"
        Expect.isTrue (File.Exists outputPath) (sprintf "output assembly '%s' not found" outputPath)
      )

    ]

  let sdkIntegrationMocks =
    testList "sdk integration" [

      testCase |> withLog "check invocation" (fun _ fs ->
        let testDir = inDir fs "sdkint_invocation"
        copyDirFromAssets fs ``samples2 library``.ProjDir testDir

        let projPath = testDir/ (``samples2 library``.ProjectFile)

        fs.mkdir_p (testDir/"mocktool")
        copyDirFromAssets fs ``mock write args``.ProjDir (testDir/"mocktool")
        let falanxMock = testDir/"mocktool"/``mock write args``.FileName
        let falanxMockArgsPath = testDir/"mocktool"/"falanx-args.txt"

        fs.cd testDir
        dotnet fs ["build"; projPath; sprintf "/p:FalanxSdk_GeneratorExe=%s" falanxMock; "/p:FalanxSdk_GeneratorExeHost=" ]
        |> ignore

        Expect.isTrue (File.Exists falanxMockArgsPath) "mock should create a file who contains the args of invocation"

        let expected =
          [ sprintf """ --inputfile "%s" """ (testDir/``samples2 library``.ProtoFile)
            sprintf """ --outputfile "%s" """ (testDir/"l1.Contracts"/"obj"/"Debug"/"netstandard2.0"/"l1.Contracts.FalanxSdk.g.fs")
            sprintf """ --defaultnamespace "l1.Contracts" """ ]
          |> List.map (fun s -> s.Trim())
          |> String.concat " "

        let lines =
          File.ReadAllText falanxMockArgsPath
          |> fun s -> s.Trim()

        Expect.equal lines expected "check invocation args"
      )

    ]

  [ generalTests
    sanityChecks
    sdkIntegrationMocks ]
  |> testList "suite"
  |> testSequenced
