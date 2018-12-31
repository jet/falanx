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

let stdOutLines (cmd: Command) =
    cmd.Result.StandardOutput
    |> fun s -> s.Trim()
    |> fun s -> s.Split(Environment.NewLine)
    |> List.ofArray

let checkExitCodeZero (cmd: Command) =
    "command finished with exit code non-zero"
    |> Expect.equal cmd.Result.ExitCode 0

let checkExitNonCodeZero (cmd: Command) =
    "command finished with exit code zero"
    |> Expect.notEqual cmd.Result.ExitCode 0

let dotnetCmd (fs: FileUtils) args =
    fs.shellExecRun "dotnet" args

let dotnet (fs: FileUtils) args =
    fs.shellExecRun "dotnet" (args @ ["/v:n"; "/bl"])

let getEnv name =
  System.Environment.GetEnvironmentVariable(name)
  |> Option.ofObj

let getTempFilePath () =
  let tempFile = Path.GetTempFileName()
  File.Delete(tempFile)
  tempFile

let sbtPath () =
  match getEnv "SBT_HOME" with
  | None -> None
  | Some dir -> Some (dir/"sbt.bat")

let sbt_run (fs: FileUtils) args =
    match sbtPath () with
    | None -> failwithf "expected SBT_HOME env var"
    | Some sbt ->
      //sbt run with args is like
      //  sbt "run a b"
      let all = args |> Seq.ofList |> String.concat " "
      fs.shellExecRun sbt ["--warn"; sprintf "run %s" all]

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
        "    <Version></Version>"
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

    let fileNames =
      example.FileNames
      |> List.filter (fun (lang, _, _) -> lang = template.Language)
      |> List.filter (fun (_, f, _) -> template.RequiredFormats |> List.contains f)
      |> List.map (fun (_, _, path) -> path)

    // copy the example .fs file in the program dir
    for fileName in fileNames do
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

  let generalTests =
    testList "general" [
      testCase |> withLog "can show help" (fun _ fs ->

        falanx fs ["--help"]
        |> checkExitCodeZero
      )

      testCase |> withLog "failure if invalid compiler args" (fun _ fs ->

        let cmd = falanx fs ["invalid"; "compiler"; "--args"]

        "invalid args is exit code 2"
        |> Expect.equal cmd.Result.ExitCode 2
      )

      testCase |> withLog "failure if file doesnt exists" (fun _ fs ->

        let tempFile = getTempFilePath()

        let cmd = falanx fs ["--inputfile"; "doesntexists.proto"; "--outputfile"; tempFile]

        "non existing proto file is exit code 4"
        |> Expect.equal cmd.Result.ExitCode 4
      )

      testCase |> withLog "failure if proto file cannot be parsed" (fun _ fs ->

        let tempFile = getTempFilePath()

        let protoFile = sprintf "%s.proto" (getTempFilePath())

        // take a valid proto file  and make it invalid
        File.ReadAllText( (ExamplesDir/``sample6 bundle``.ExampleDir/``sample6 bundle``.ProtoFile) )
        |> fun text -> text.Replace("message", "messaAAAgEEE")
        |> fun text -> File.WriteAllText(protoFile, text)

        let cmd = falanx fs ["--inputfile"; protoFile; "--outputfile"; tempFile]

        "existing but invalid proto file is exit code 5"
        |> Expect.equal cmd.Result.ExitCode 5
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

      let binaryFilePath = testDir/"my.bin"
      let textFilePath = testDir/"output.txt"

      dotnetCmd fs [outputPath; "--serialize"; binaryFilePath]
      |> checkExitCodeZero

      "check serialized binary file exists"
      |> Expect.isTrue (File.Exists binaryFilePath)

      dotnetCmd fs [outputPath; "--deserialize"; binaryFilePath; "--out"; textFilePath]
      |> checkExitCodeZero

      "check deserialized output text file exists"
      |> Expect.isTrue (File.Exists textFilePath)

      let text = File.ReadAllText(textFilePath)

      "check deserialized text exists"
      |> Expect.isNotWhitespace text

  let sanityChecks =
    testList "sanity check of projects" [

      testCase |> withLog "can build sample2 binary" (fun _ fs ->
        let testDir = inDir fs "sanity_check_sample2"

        testDir
        |> buildExampleWithTemplate fs ``template1 binary`` ``sample6 bundle``
      )

      testCase |> withLog "can build sample3 json" (fun _ fs ->
        let testDir = inDir fs "sanity_check_sample3_json"

        Tests.skiptest "schema with collection and json format doesnt work yet"

        testDir
        |> buildExampleWithTemplate fs ``template2 json`` ``sample6 bundle``
      )

      testCase |> withLog "can build sample4 binary+json" (fun _ fs ->
        let testDir = inDir fs "sanity_check_sample4_binaryjson"

        Tests.skiptest "schema with collection and json format doesnt work yet"

        testDir
        |> buildExampleWithTemplate fs ``template3 binary+json`` ``sample6 bundle``
      )

      testCase |> withLog "can build sample7 json" (fun _ fs ->
        let testDir = inDir fs "sanity_check_sample7_json"

        testDir
        |> buildExampleWithTemplate fs ``template2 json`` ``sample7 itemLevelOrderHistory``
      )

    ]

  let sdkIntegrationMocks =

    let replaceRealFalanxEnvVar realPath mockFilename =
        let replaceText f path =
          let content = File.ReadAllText(path)
          let newContent = f content
          File.WriteAllText(path, newContent)

        (sprintf "%s.bat" mockFilename)
        |> replaceText (fun text -> text.Replace("%REAL_FALANX%", realPath))

        mockFilename
        |> replaceText (fun text -> text.Replace("$REAL_FALANX", realPath))

    let dotnetBuildWithFalanxArgsMockAndArgs (fs: FileUtils) testDir args projPath =

        fs.mkdir_p (testDir/"mocktool")
        copyDirFromAssets fs ``mock write args``.ProjDir (testDir/"mocktool")
        let falanxMock = testDir/"mocktool"/``mock write args``.FileName
        let falanxMockArgsPath = testDir/"mocktool"/"falanx-args.txt"

        falanxMock |> replaceRealFalanxEnvVar (TestRunDirToolDir/"bin"/"falanx")

        fs.rm_rf falanxMockArgsPath

        fs.cd testDir

        let cmd = dotnet fs (["build"; projPath; sprintf "/p:FalanxSdk_GeneratorExe=%s" falanxMock; "/p:FalanxSdk_GeneratorExeHost=" ] @ args)

        let lines =
          if File.Exists falanxMockArgsPath then
            File.ReadLines falanxMockArgsPath
            |> List.ofSeq
          else
            []

        cmd, lines

    let dotnetBuildWithFalanxArgsMock fs testDir projPath =
      dotnetBuildWithFalanxArgsMockAndArgs fs testDir [] projPath

    testList "sdk integration" [

      testCase |> withLog "check invocation binary" (fun _ fs ->
        let testDir = inDir fs "sdkint_invocation_binary"

        testDir
        |> copyExampleWithTemplate fs ``template1 binary`` ``sample6 bundle``

        let projPath = testDir/ (``template1 binary``.ProjectFile)

        let cmd, lines = dotnetBuildWithFalanxArgsMock fs testDir projPath

        let expected =
          [ "--inputfile"
            (testDir/``template1 binary``.ProtoFile)
            "--outputfile"
            sprintf "%s.fs" (testDir/``template1 binary``.ProtoFile)
            "--defaultnamespace"
            "l1.Contracts"
            "--serializer"
            "binary" ]

        Expect.equal lines expected "check invocation args"

        cmd |> checkExitCodeZero
      )

      testCase |> withLog "check invocation json" (fun _ fs ->
        let testDir = inDir fs "sdkint_invocation_json"

        testDir
        |> copyExampleWithTemplate fs ``template2 json`` ``sample6 bundle``

        let projPath = testDir/ (``template2 json``.ProjectFile)

        let _, lines = dotnetBuildWithFalanxArgsMock fs testDir projPath

        let expected =
          [ "--inputfile"
            (testDir/``template2 json``.ProtoFile)
            "--outputfile"
            sprintf "%s.fs" (testDir/``template2 json``.ProtoFile)
            "--defaultnamespace"
            "l1.Contracts"
            "--serializer"
            "json" ]

        Expect.equal lines expected "check invocation args"
      )

      testCase |> withLog "check invocation binary+json" (fun _ fs ->
        let testDir = inDir fs "sdkint_invocation_binaryjson"

        testDir
        |> copyExampleWithTemplate fs ``template3 binary+json`` ``sample6 bundle``

        let projPath = testDir/ (``template3 binary+json``.ProjectFile)

        let _, lines = dotnetBuildWithFalanxArgsMock fs testDir projPath

        let expected =
          [ "--inputfile"
            (testDir/``template3 binary+json``.ProtoFile)
            "--outputfile"
            sprintf "%s.fs" (testDir/``template3 binary+json``.ProtoFile)
            "--defaultnamespace"
            "l1.Contracts"
            "--serializer"
            "json"
            "--serializer"
            "binary" ]

        Expect.equal lines expected "check invocation args"
      )

      testCase |> withLog "check custom namespace" (fun _ fs ->
        let testDir = inDir fs "sdkint_custom_namespace"

        testDir
        |> copyExampleWithTemplate fs ``template1 binary`` ``sample6 bundle``

        let projPath = testDir/ (``template1 binary``.ProjectFile)

        let ns = "abcd"

        let _, lines = dotnetBuildWithFalanxArgsMockAndArgs fs testDir [sprintf "/p:RootNamespace=%s" ns] projPath

        let expected =
          [ "--inputfile"
            (testDir/``template1 binary``.ProtoFile)
            "--outputfile"
            sprintf "%s.fs" (testDir/``template1 binary``.ProtoFile)
            "--defaultnamespace"
            ns
            "--serializer"
            "binary" ]

        Expect.equal lines expected "check custom namespace"
      )

      testCase |> withLog "check no double generation" (fun _ fs ->
        let testDir = inDir fs "sdkint_no_regen"

        testDir
        |> copyExampleWithTemplate fs ``template1 binary`` ``sample6 bundle``

        let projPath = testDir/ (``template1 binary``.ProjectFile)

        let cmd, lines = dotnetBuildWithFalanxArgsMock fs testDir projPath

        let expected =
          [ "--inputfile"
            (testDir/``template1 binary``.ProtoFile)
            "--outputfile"
            sprintf "%s.fs" (testDir/``template1 binary``.ProtoFile)
            "--defaultnamespace"
            "l1.Contracts"
            "--serializer"
            "binary" ]

        Expect.equal lines expected "check invocation args"

        cmd |> checkExitCodeZero

        let cmd, lines = dotnetBuildWithFalanxArgsMock fs testDir projPath

        Expect.equal lines [] "check is not invoked the second time"

        cmd |> checkExitCodeZero
      )

      testCase |> withLog "check regen if proto file is changed" (fun _ fs ->
        let testDir = inDir fs "sdkint_proto_changed"

        testDir
        |> copyExampleWithTemplate fs ``template1 binary`` ``sample6 bundle``

        let projPath = testDir/ (``template1 binary``.ProjectFile)

        let cmd, lines = dotnetBuildWithFalanxArgsMock fs testDir projPath

        let expected =
          [ "--inputfile"
            (testDir/``template1 binary``.ProtoFile)
            "--outputfile"
            sprintf "%s.fs" (testDir/``template1 binary``.ProtoFile)
            "--defaultnamespace"
            "l1.Contracts"
            "--serializer"
            "binary" ]

        Expect.equal lines expected "check invocation args"

        cmd |> checkExitCodeZero

        fs.touch (testDir/``template1 binary``.ProtoFile)

        let cmd, lines = dotnetBuildWithFalanxArgsMock fs testDir projPath

        Expect.equal lines expected "check is invoked the second time"

        cmd |> checkExitCodeZero
      )

      testCase |> withLog "proto file changed and now invalid make the build fail" (fun _ fs ->
        let testDir = inDir fs "sdkint_now_invalid_fails"

        testDir
        |> copyExampleWithTemplate fs ``template1 binary`` ``sample6 bundle``

        let projPath = testDir/ (``template1 binary``.ProjectFile)

        let cmd, lines = dotnetBuildWithFalanxArgsMock fs testDir projPath

        let protoFile = testDir/``template1 binary``.ProtoFile

        let expected =
          [ "--inputfile"
            protoFile
            "--outputfile"
            sprintf "%s.fs" (testDir/``template1 binary``.ProtoFile)
            "--defaultnamespace"
            "l1.Contracts"
            "--serializer"
            "binary" ]

        Expect.equal lines expected "check invocation args"

        cmd |> checkExitCodeZero

        // make the .proto file invalid
        File.ReadAllText(protoFile)
        |> fun content -> content.Replace("message", "MESSAGGIO")
        |> fun content -> File.WriteAllText(protoFile, content)

        let cmd, lines = dotnetBuildWithFalanxArgsMock fs testDir projPath

        Expect.equal lines expected "check is invoked the second time"

        cmd |> checkExitNonCodeZero
      )

    ]

  let interop =
  
    let requireSbt () =
      if sbtPath () |> Option.isNone then
        Tests.skiptest "sbt now found, check if env var SBT_HOME is set"

    testList "interop" [

      testCase |> withLog "scala sanity check" (fun _ fs ->
        let testDir = inDir fs "sanity_check_scala"

        let binaryFilePath = testDir/"a.bin"
        let outFilePath = testDir/"out.txt"

        // copy the template and add the sample
        testDir
        |> copyExampleWithTemplate fs ``template4 scala`` ``sample7 itemLevelOrderHistory``

        requireSbt ()

        fs.cd testDir

        // serialize
        sbt_run fs ["--serialize"; binaryFilePath]
        |> checkExitCodeZero

        "check serialized file exists"
        |> Expect.isTrue (File.Exists binaryFilePath)

        // deserialize
        sbt_run fs ["--deserialize"; binaryFilePath; "--out"; outFilePath]
        |> checkExitCodeZero

        "check out file exists"
        |> Expect.isTrue (File.Exists outFilePath)

        let text = File.ReadAllText(outFilePath)

        "check deserialize"
        |> Expect.equal text "ItemLevelOrderHistory(client1,sku1,12.3,brandA,product1,45.6)"
      )

      testCase |> withLog ".net -> binary -> scala" (fun _ fs ->
        let testDir = inDir fs "interop_net_scala"

        let scalaApp = testDir/"scala-app"
        let netApp = testDir/"net-app"

        let binaryFilePath = testDir/"a.bin"
        let outFilePath = testDir/"out.txt"

        // copy the template and add the sample
        scalaApp
        |> copyExampleWithTemplate fs ``template4 scala`` ``sample7 itemLevelOrderHistory``

        netApp
        |> copyExampleWithTemplate fs ``template1 binary`` ``sample7 itemLevelOrderHistory``

        // serialize with .net
        fs.cd netApp

        dotnetCmd fs ["run"; "-p"; ``template1 binary``.AssemblyName; "--"; "--serialize"; binaryFilePath]
        |> checkExitCodeZero

        "check serialized file exists"
        |> Expect.isTrue (File.Exists binaryFilePath)

        // deserialize with scala
        requireSbt ()

        fs.cd scalaApp

        sbt_run fs ["--deserialize"; binaryFilePath; "--out"; outFilePath]
        |> checkExitCodeZero

        "check out file exists"
        |> Expect.isTrue (File.Exists outFilePath)

        let text = File.ReadAllText(outFilePath)

        "check deserialize"
        |> Expect.equal text "ItemLevelOrderHistory(clientA,sku12345,78.91,myBrand1,p100,43.21)"
      )

      testCase |> withLog "scala -> binary -> .net" (fun _ fs ->
        let testDir = inDir fs "interop_scala_net"

        let scalaApp = testDir/"scala-app2"
        let netApp = testDir/"net-app2"

        let binaryFilePath = testDir/"b.bin"
        let outFilePath = testDir/"out2.txt"

        // copy the template and add the sample
        scalaApp
        |> copyExampleWithTemplate fs ``template4 scala`` ``sample7 itemLevelOrderHistory``

        netApp
        |> copyExampleWithTemplate fs ``template1 binary`` ``sample7 itemLevelOrderHistory``

        // serialize with scala
        requireSbt ()

        fs.cd scalaApp

        sbt_run fs ["--serialize"; binaryFilePath]
        |> checkExitCodeZero

        "check out file exists"
        |> Expect.isTrue (File.Exists binaryFilePath)

        // deserialize with .net
        fs.cd netApp

        dotnetCmd fs ["run"; "-p"; ``template1 binary``.AssemblyName; "--"; "--deserialize"; binaryFilePath; "--out"; outFilePath]
        |> checkExitCodeZero

        "check deserialized file exists"
        |> Expect.isTrue (File.Exists outFilePath)

        let textLines = File.ReadAllLines(outFilePath) |> List.ofArray

        let expected =
          ["""{clientId = Some "client1";"""
           """ retailSkuId = Some "sku1";"""
           """ categoryId = Some 12.3;"""
           """ brand = Some "brandA";"""
           """ product = Some "product1";"""
           """ orderTss = Some 45.5999985f;}""" ]

        "check deserialize"
        |> Expect.equal textLines expected
      )

      testCase |> withLog "scala json sanity check" (fun _ fs ->
        let testDir = inDir fs "sanity_check_scala_json"

        let jsonFilePath = testDir/"a.txt"
        let outFilePath = testDir/"out.txt"

        // copy the template and add the sample
        testDir
        |> copyExampleWithTemplate fs ``template5 scala json`` ``sample7 itemLevelOrderHistory``

        requireSbt ()

        fs.cd testDir

        // serialize
        sbt_run fs ["--serialize"; jsonFilePath]
        |> checkExitCodeZero

        "check serialized file exists"
        |> Expect.isTrue (File.Exists jsonFilePath)

        // deserialize
        sbt_run fs ["--deserialize"; jsonFilePath; "--out"; outFilePath]
        |> checkExitCodeZero

        "check out file exists"
        |> Expect.isTrue (File.Exists outFilePath)

        let text = File.ReadAllText(outFilePath)

        "check deserialize"
        |> Expect.equal text "ItemLevelOrderHistory(client1,sku1,12.3,brandA,product1,45.6)"
      )

      testCase |> withLog ".net -> json -> scala" (fun _ fs ->
        let testDir = inDir fs "interop_net_scala_json"

        let scalaApp = testDir/"scala-app"
        let netApp = testDir/"net-app"

        let jsonFilePath = testDir/"a.json"
        let outFilePath = testDir/"out.txt"

        // copy the template and add the sample
        scalaApp
        |> copyExampleWithTemplate fs ``template5 scala json`` ``sample7 itemLevelOrderHistory``

        netApp
        |> copyExampleWithTemplate fs ``template2 json`` ``sample7 itemLevelOrderHistory``

        // serialize with .net
        fs.cd netApp

        dotnetCmd fs ["run"; "-p"; ``template2 json``.AssemblyName; "--"; "--serialize"; jsonFilePath]
        |> checkExitCodeZero

        "check serialized file exists"
        |> Expect.isTrue (File.Exists jsonFilePath)

        // deserialize with scala
        requireSbt ()

        fs.cd scalaApp

        sbt_run fs ["--deserialize"; jsonFilePath; "--out"; outFilePath]
        |> checkExitCodeZero

        "check out file exists"
        |> Expect.isTrue (File.Exists outFilePath)

        let text = File.ReadAllText(outFilePath)

        "check deserialize"
        |> Expect.equal text "ItemLevelOrderHistory(clientA,sku12345,78.91,myBrand1,p100,43.21)"
      )

      testCase |> withLog "scala -> json -> .net" (fun _ fs ->
        let testDir = inDir fs "interop_scala_net_json"

        let scalaApp = testDir/"scala-app2"
        let netApp = testDir/"net-app2"

        let jsonFilePath = testDir/"b.json"
        let outFilePath = testDir/"out2.txt"

        // copy the template and add the sample
        scalaApp
        |> copyExampleWithTemplate fs ``template5 scala json`` ``sample7 itemLevelOrderHistory``

        netApp
        |> copyExampleWithTemplate fs ``template2 json`` ``sample7 itemLevelOrderHistory``

        // serialize with scala
        requireSbt ()

        fs.cd scalaApp

        sbt_run fs ["--serialize"; jsonFilePath]
        |> checkExitCodeZero

        "check out file exists"
        |> Expect.isTrue (File.Exists jsonFilePath)

        // deserialize with .net
        fs.cd netApp

        dotnetCmd fs ["run"; "-p"; ``template2 json``.AssemblyName; "--"; "--deserialize"; jsonFilePath; "--out"; outFilePath]
        |> checkExitCodeZero

        "check deserialized file exists"
        |> Expect.isTrue (File.Exists outFilePath)

        let textLines = File.ReadAllLines(outFilePath) |> List.ofArray

        let expected =
          ["""{clientId = Some "client1";"""
           """ retailSkuId = Some "sku1";"""
           """ categoryId = Some 12.3;"""
           """ brand = Some "brandA";"""
           """ product = Some "product1";"""
           """ orderTss = Some 45.5999985f;}""" ]

        "check deserialize"
        |> Expect.equal textLines expected
      )

    ]

  let template =

    let uninstallTemplate fs =
      dotnetCmd fs ["new"; "-u"; "Falanx.Templates" ]

    let installTemplate fs =
      dotnetCmd fs ["new"; "--nuget-source"; NupkgsDir; "-i"; sprintf "Falanx.Templates::%s" pkgUnderTestVersion ]

    let withTemplate fs =
      // uninstall if installed
      uninstallTemplate fs |> ignore

      installTemplate fs
      |> checkExitCodeZero

      { new IDisposable with
          member __.Dispose() =
            // uninstall if installed
            uninstallTemplate fs |> ignore
      }

    testList "template" [

      testCase |> withLog "codec all" (fun _ fs ->
        let testDir = inDir fs "template_codec_all"

        use _template = withTemplate fs

        dotnetCmd fs ["new"; "falanx"; "--codec"; "all" ]
        |> checkExitCodeZero

        dotnet fs ["build" ]
        |> checkExitCodeZero
      )

      testCase |> withLog "codec binary" (fun _ fs ->
        let testDir = inDir fs "template_codec_binary"

        use _template = withTemplate fs

        dotnetCmd fs ["new"; "falanx"; "--codec"; "binary" ]
        |> checkExitCodeZero

        dotnet fs ["build" ]
        |> checkExitCodeZero
      )

      testCase |> withLog "codec json" (fun _ fs ->
        let testDir = inDir fs "template_codec_json"

        use _template = withTemplate fs

        dotnetCmd fs ["new"; "falanx"; "--codec"; "json" ]
        |> checkExitCodeZero

        dotnet fs ["build" ]
        |> checkExitCodeZero
      )

      testCase |> withLog "default codec" (fun _ fs ->
        let testDir = inDir fs "template_codec_default"

        use _template = withTemplate fs

        dotnetCmd fs ["new"; "falanx" ]
        |> checkExitCodeZero

        dotnet fs ["build" ]
        |> checkExitCodeZero
      )
    ]

  [ generalTests
    sanityChecks
    sdkIntegrationMocks
    template
    interop ]
  |> testList "suite"
  |> testSequenced
