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

let dotnetCmd (fs: FileUtils) args =
    fs.shellExecRun "dotnet" args

let dotnet (fs: FileUtils) args =
    fs.shellExecRun "dotnet" (args @ ["/v:n"; "/bl"])

let sbt_run (fs: FileUtils) args =
    let sbtPath = @"C:\tools\sbt-1.2.6\bin\sbt.bat"
    //sbt run with args is like
    //  sbt "run a b"
    let all = args |> Seq.ofList |> String.concat " "
    fs.shellExecRun sbtPath ["--warn"; sprintf "run %s" all]

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
    dotnetCmd fs ["tool"; "install"; "Falanx.Tool"; "--version"; pkgUnderTestVersion; "--tool-path"; (TestRunDirToolDir/"bin"); "--configfile"; (TestRunDirToolDir/"nuget"/"nuget.config") ]
    |> checkExitCodeZero

let falanx (fs: FileUtils) args =
    fs.shellExecRun (TestRunDirToolDir/"bin"/"falanx") args

let copyDirFromAssets (fs: FileUtils) source outDir =
    fs.mkdir_p outDir

    let path = ExamplesDir/source

    fs.cp_r path outDir

let copyExampleWithTemplate (fs: FileUtils) (template: TestAssetProjInfo) (example: TestAssetExampleInfo) outDir =
    fs.mkdir_p outDir

    // copy all the template files
    fs.cp_r (ExamplesDir/template.ProjDir) outDir

    // copy the example .fs file in the program dir
    for fileName in example.FileNames do
      fs.cp (ExamplesDir/example.ExampleDir/fileName) (outDir/template.AssemblyName)

    // copy the example proto
    fs.mkdir_p (outDir/template.ProtoFile |> Path.GetDirectoryName)
    fs.cp (ExamplesDir/example.ExampleDir/example.ProtoFile) (outDir/template.ProtoFile)

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

  let buildExampleWithTemplate fs template sample testDir =
      // copy the template and add the sample
      testDir
      |> copyExampleWithTemplate fs template sample

      let projPath = testDir/ (template.ProjectFile)
      let projDir = Path.GetDirectoryName projPath

      // should build correctly
      fs.cd testDir
      dotnet fs ["build"; projPath]
      |> checkExitCodeZero

      // should run correctly
      let outputPath = projDir/"bin"/"Debug"/"netcoreapp2.1"/template.AssemblyName + ".dll"
      Expect.isTrue (File.Exists outputPath) (sprintf "output assembly '%s' not found" outputPath)

      dotnet fs [outputPath]
      |> checkExitCodeZero

  let sanityChecks =
    testList "sanity check of projects" [

      testCase |> withLog "can build sample2 binary" (fun _ fs ->
        let testDir = inDir fs "sanity_check_sample2"

        testDir
        |> buildExampleWithTemplate fs ``template1 binary`` ``sample2 binary``
      )

      testCase |> withLog "can build sample3 json" (fun _ fs ->
        let testDir = inDir fs "sanity_check_sample3_json"

        Tests.skiptest "only json doesnt work yet"

        testDir
        |> buildExampleWithTemplate fs ``template2 json`` ``sample3 json``
      )

      testCase |> withLog "can build sample4 binary+json" (fun _ fs ->
        let testDir = inDir fs "sanity_check_sample4_binaryjson"

        Tests.skiptest "doesnt contains a json serialization/deserialization"

        testDir
        |> buildExampleWithTemplate fs ``template3 binary+json`` ``sample4 binary+json``
      )

      testCase |> withLog "can build sample5 pkg" (fun _ fs ->
        let testDir = inDir fs "sanity_check_sample5"

        testDir
        |> buildExampleWithTemplate fs ``template1 binary`` ``sample5 pkg``
      )

    ]

  let sdkIntegrationMocks =

    let dotnetBuildWithFalanxArgsMock (fs: FileUtils) testDir projPath =

        fs.mkdir_p (testDir/"mocktool")
        copyDirFromAssets fs ``mock write args``.ProjDir (testDir/"mocktool")
        let falanxMock = testDir/"mocktool"/``mock write args``.FileName
        let falanxMockArgsPath = testDir/"mocktool"/"falanx-args.txt"

        fs.cd testDir

        dotnet fs ["build"; projPath; sprintf "/p:FalanxSdk_GeneratorExe=%s" falanxMock; "/p:FalanxSdk_GeneratorExeHost=" ]
        |> ignore

        Expect.isTrue (File.Exists falanxMockArgsPath) "mock should create a file who contains the args of invocation"

        let lines =
          File.ReadLines falanxMockArgsPath
          |> List.ofSeq

        lines

    testList "sdk integration" [

      testCase |> withLog "check invocation binary" (fun _ fs ->
        let testDir = inDir fs "sdkint_invocation_binary"
        copyDirFromAssets fs ``template1 binary``.ProjDir testDir

        let projPath = testDir/ (``template1 binary``.ProjectFile)

        let lines = dotnetBuildWithFalanxArgsMock fs testDir projPath

        let expected =
          [ "--inputfile"
            (testDir/``template1 binary``.ProtoFile)
            "--outputfile"
            (testDir/"l1.Contracts"/"obj"/"Debug"/"netstandard2.0"/"l1.Contracts.FalanxSdk.g.fs")
            "--defaultnamespace"
            "l1.Contracts"
            "--serializer"
            "binary" ]

        Expect.equal lines expected "check invocation args"
      )

      testCase |> withLog "check invocation json" (fun _ fs ->
        let testDir = inDir fs "sdkint_invocation_json"
        copyDirFromAssets fs ``template2 json``.ProjDir testDir

        let projPath = testDir/ (``template2 json``.ProjectFile)

        let lines = dotnetBuildWithFalanxArgsMock fs testDir projPath

        let expected =
          [ "--inputfile"
            (testDir/``template2 json``.ProtoFile)
            "--outputfile"
            (testDir/"l1.Contracts"/"obj"/"Debug"/"netstandard2.0"/"l1.Contracts.FalanxSdk.g.fs")
            "--defaultnamespace"
            "l1.Contracts"
            "--serializer"
            "json" ]

        Expect.equal lines expected "check invocation args"
      )

      testCase |> withLog "check invocation binary+json" (fun _ fs ->
        let testDir = inDir fs "sdkint_invocation_binaryjson"
        copyDirFromAssets fs ``template3 binary+json``.ProjDir testDir

        let projPath = testDir/ (``template3 binary+json``.ProjectFile)

        let lines = dotnetBuildWithFalanxArgsMock fs testDir projPath

        let expected =
          [ "--inputfile"
            (testDir/``template3 binary+json``.ProtoFile)
            "--outputfile"
            (testDir/"l1.Contracts"/"obj"/"Debug"/"netstandard2.0"/"l1.Contracts.FalanxSdk.g.fs")
            "--defaultnamespace"
            "l1.Contracts"
            "--serializer"
            "json"
            "--serializer"
            "binary" ]

        Expect.equal lines expected "check invocation args"
      )

    ]

  let interop =
    testList "interop" [

      ftestCase |> withLog "scala" (fun _ fs ->
        let testDir = inDir fs "interop_scala"

        // copy the template and add the sample
        testDir
        |> copyExampleWithTemplate fs ``template4 scala`` ``sample7 scala binary``

        fs.cd testDir

        let binaryFilePath = testDir/"a.bin"

        // serialize
        sbt_run fs ["--serialize"; binaryFilePath]
        |> checkExitCodeZero
        
        // deserialize
        let r = sbt_run fs ["--deserialize"; binaryFilePath]
        r |> checkExitCodeZero

        "check deserialize"
        |> Expect.equal (stdOutLines r) ["ItemLevelOrderHistory(client1,sku1,12.3,brandA,product1,45.6)"]
      )
    ]

  [ generalTests
    sanityChecks
    sdkIntegrationMocks
    interop ]
  |> testList "suite"
  |> testSequenced
