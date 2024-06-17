module Fone.Cli.Cli
open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open Fable
open Fable.AST
open Fable.Cli.Main
open Fable.Compiler.Util
open Fable.Transforms.State
open Fone
open Thoth.Json.Net


module Compiler =
    let options: Fable.CompilerOptions = {
        TypedArrays = true
        ClampByteArrays = false
        Language = Language.Plugin "C"
        Define = []
        DebugMode = true
        OptimizeFSharpAst = false // todo: TRUE
        Verbosity = Verbosity.Normal
        FileExtension = ".c"
        TriggeredByDependency = false
        NoReflection = true // todo: false
    }
    let isSilent = false
    let cliArgs projFile options : CliArgs = {
        ProjectFile = projFile
        RootDir = Path.GetDirectoryName(projFile)
        OutDir = None
        IsWatch = true
        Precompile = false
        PrecompiledLib = None
        PrintAst = true
        FableLibraryPath = None
        Configuration = ""
        NoRestore = true
        NoCache = true
        NoParallelTypeCheck = false // todo: true
        SourceMaps = false
        SourceMapsRoot = None
        Exclude = [ "Fable.Core" ]
        Replace = Map.empty
        RunProcess = None
        CompilerOptions = options
    }
    // let compileProject projFile =
    //     let projCracked = ProjectCracked.Init(cliArgs projFile options)
    //     let checker = InteractiveChecker.Create(projCracked.ProjectOptions)
    //     let fsharpAssemblies = checker.GetImportedAssemblies() |> Async.RunSynchronously
    //     let filePaths, sourceReader = Fable.Compiler.File.MakeSourceReader projCracked.SourceFiles
    //     let results = checker.ParseAndCheckProject(projCracked.ProjectFile, filePaths, sourceReader, Array.last projCracked.SourceFilePaths, ignore) |> Async.RunSynchronously
    //     ()
    let createProject (projFile: string) =
        let projCracked = ProjectCracked.Init(cliArgs projFile options)
        let checker = InteractiveChecker.Create(projCracked.ProjectOptions)
        let fsharpAssemblies = checker.GetImportedAssemblies() |> Async.RunSynchronously
        let filePaths, sourceReader = Fable.Compiler.File.MakeSourceReader projCracked.SourceFiles
        let results = checker.ParseAndCheckProject(projCracked.ProjectFile, filePaths, sourceReader, Array.last projCracked.SourceFilePaths, ignore) |> Async.RunSynchronously

        let project = Project.From (projFile, projCracked.SourceFilePaths, results.AssemblyContents.ImplementationFiles, fsharpAssemblies)
        let fableLibDir = ""

        let cache =  Fone.Database.FableCompilationCache()
        // Fable.C.Helpers.database.Value <- cache
        // let filePath = Array.last projCracked.SourceFilePaths
        let compiledFiles = [|
            for filePath in projCracked.SourceFilePaths do
                CompilerImpl(filePath, project, options, fableLibDir)
        |]
        projCracked, project, compiledFiles, cache
    let compile = ()
    let compileProject (projFile: string) =
        // let pathResolver = {
        //     new PathResolver with
        //         member this.TryPrecompiledOutPath (sourceDir, relativePath) = None
        //         member this.GetOrAddDeduplicateTargetDir (importDir, addTargetDir) = ""
        // }
        printfn $"Compiling {projFile}"
        let projCracked = ProjectCracked.Init(cliArgs projFile options)
        let checker = InteractiveChecker.Create(projCracked.ProjectOptions)
        let fsharpAssemblies = checker.GetImportedAssemblies() |> Async.RunSynchronously
        let filePaths, sourceReader = Fable.Compiler.File.MakeSourceReader projCracked.SourceFiles
        let results = checker.ParseAndCheckProject(projCracked.ProjectFile, filePaths, sourceReader, Array.last projCracked.SourceFilePaths, ignore) |> Async.RunSynchronously

        let project = Project.From(projFile, projCracked.SourceFilePaths, results.AssemblyContents.ImplementationFiles, fsharpAssemblies)
        let fableLibDir = ""

        let cache =  Fone.Database.FableCompilationCache()
        // Fable.C.Helpers.database.Value <- cache
        // let filePath = Array.last projCracked.SourceFilePaths
        let compiledFiles = [|
            for filePath in [| "main.fsx" |] do
                let com = CompilerImpl(filePath, project, options, fableLibDir)
                let file = Fable.Transforms.FSharp2Fable.Compiler.transformFile com
                cache.UpdateWithFile com file
                let transformedFile = Fable.Transforms.FableTransforms.transformFile com file
                let c_file = Fable.C.File.transformFile true cache com transformedFile
                Fable.C.File.writeFile filePath c_file.includes c_file.compiledModule c_file.static_constructor
        |]
        let header =
            let com = CompilerImpl(projCracked.SourceFilePaths |> Seq.last, project, options, fableLibDir)
            Fable.C.ProjectWriter.writeModuleHeaderFile
                {
                    com = com
                    currentFile = "Program.fs"; idents = []
                    db = cache
                    // todo:
                    file = Unchecked.defaultof<_>
                    m = Operators.Unchecked.defaultof<_>
                    useSourceMap = true
                }
                "/build/project.json"
        let files = io.files
        let generics = files |> Seq.find (fun kv -> kv.Key.Contains ".generics.")
        let output =
            compiledFiles |> String.concat "\n"
        let compiledOutput =
            $"{header.file}\n{header.generics}\n{output}\n{header.init}"
            |> _.Replace("\r\n", "\n").Replace("\r", "")
        let outputPath = projFile.Replace (".fsproj", ".fs.c")
        File.WriteAllText (outputPath, compiledOutput)

let serialization () =
    let testsProjFile =
        Path.Join(__SOURCE_DIRECTORY__, "../tests/C/Fable.Tests.C/Fable.Tests.C.fsproj")
        // Path.Join(__SOURCE_DIRECTORY__, "../src/quicktest/Quicktest.fsproj")
        // "C:/Users/Dave/projects/Fable/src/quicktest/Quicktest.fsproj"
    let ast = Fable.Expr.Value (Fable.ValueKind.UnitConstant, None)
    let s = Thoth.Json.Net.Encode.Auto.toString ast
    let _, _, compilers, cache = Compiler.createProject testsProjFile
    let files =
        compilers
        |> Array.map Fable.Transforms.FSharp2Fable.Compiler.transformFile
        |> Array.zip compilers
    let _fixed =
        files |> Array.map (fun (com, file) -> Fable.Transforms.FableTransforms.transformFile com file)
    let strings =
        let attrDecode : Decoder<Fable.Attribute> =
            // Decode.nil |> Decode.map (fun _ -> )
            // Decode.nil Unchecked.defaultof<_>
            let o = Encode.Auto.generateEncoder<obj>()
            Decode.object (fun get ->
                { new Fable.Attribute with
                    member this.Entity =
                        get.Required.Field "entity" (Decode.Auto.generateDecoder<Fable.EntityRef> ())
                    member this.ConstructorArgs = [
                        get.Required.Field "constructorArgs" (Decode.list <| Decode.Auto.generateDecoder<obj> ())
                    ]
                }
            )
        let attrEncode (attr: Fable.Attribute) =
            // Encode.nil
            // Encode.Auto.generateEncoder<EntityRef>
            let o = Encode.Auto.generateEncoder<obj>()
            Encode.object [
                "entity", Encode.Auto.generateEncoder<Fable.EntityRef> () attr.Entity
                "constructorArgs", Encode.list (attr.ConstructorArgs |> List.map (Encode.Auto.generateEncoder<obj> ()))
            ]
            // Encode.list (attr.ConstructorArgs |> List.map o)
        let extra =
            Extra.empty
            // |> Extra.withCustom seqEncode seqDecode
            |> Extra.withCustom attrEncode attrDecode
        let seqDecode : Decoder<Fable.Attribute seq> =
            // let foo = List.ofSeq foo
            // let asdf = Decode.Auto.generateDecoder<Fable.Attribute> (extra = extra)
            let asdf : Decoder<_> = attrDecode
            // let items = foo |> List.map asdf
            // Decode.list asdf
            Decode.map Seq.ofList (Decode.list asdf)
        let seqEncode (foo: Fable.Attribute seq) =
            let foo = List.ofSeq foo
            // let asdf = Encode.Auto.generateEncoder<Fable.Attribute> (extra = extra)
            let asdf = attrEncode
            let items = foo |> List.map asdf
            Encode.list items
        let extra =
            Extra.empty
            |> Extra.withCustom seqEncode seqDecode
            |> Extra.withCustom attrEncode attrDecode
        let foo = Encode.Auto.generateEncoder<Fable.Declaration>(extra = extra)
        _fixed
        |> Array.map (fun file ->
            file.Declarations
            // |> List.map Encode. foo
            |> List.map (fun file -> Encode.Auto.toString (file, extra = extra))
        )
    printfn $"{s}"
let asdf () =
    let file = "/proj.fsproj"
    let text ="""
<Project Sdk="Microsoft.NET.Sdk">
    <PropertyGroup>
        <OutputType>Exe</OutputType>
        <TargetFramework>net8.0</TargetFramework>
    </PropertyGroup>

    <ItemGroup>
        <Compile Include="Program.fs"/>
    </ItemGroup>
</Project>
"""
    let code = "
module Program

printfn \"Hello world!!\"
"
    try
        System.IO.File.WriteAllText(file, text)
        System.IO.File.WriteAllText("/Program.fs", code)
        let output = File.ReadAllText(file)
        let sourceFiles = [| "/Program.fs" |]
        let opts: FSharpProjectOptions = {
            ProjectFileName = ""
            ProjectId = None
            SourceFiles = sourceFiles
            OtherOptions = [||]
            ReferencedProjects = [||]
            IsIncompleteTypeCheckEnvironment = false

            UseScriptResolutionRules = true

            LoadTime = DateTime.Now
            UnresolvedReferences = None
            OriginalLoadReferences = []
            Stamp = None
        }
        let checker = InteractiveChecker.Create(opts)
        printfn $"output = {output}"
        printfn $"checker = {checker}"
        let fsharpAssemblies =
            // checker.GetImportedAssemblies() |> Async.RunSynchronously
            []
        let fableSrcFiles = sourceFiles |> Array.map (Fable.Compiler.File)
        let filePaths, sourceReader = Fable.Compiler.File.MakeSourceReader fableSrcFiles
        task {
            let! results =
                checker.ParseAndCheckProject(file, filePaths, sourceReader, Array.last sourceFiles, ignore)
                // |> Async.RunSynchronously

            let project =
                Project.From (file, sourceFiles, results.AssemblyContents.ImplementationFiles, fsharpAssemblies)
            let fableLibDir = ""

            let cache =  Fone.Database.FableCompilationCache()
            // Fable.C.Helpers.database.Value <- cache
            // let filePath = Array.last projCracked.SourceFilePaths
            let compiledFiles = [|
                for filePath in sourceFiles do
                    CompilerImpl(filePath, project, Compiler.options, fableLibDir)
            |]
            Compiler.compileProject file
        } |> ignore
    with error ->
        printfn $"{error}"
