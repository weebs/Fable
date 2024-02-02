module Fone.Cli.Cli
open System.IO
open FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.Cli.Main
open Fable.Compiler.Util
open Fable.Transforms.State
open Fone

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
    let compileProject (projFile: string) = //async {
        let pathResolver = {
            new PathResolver with
                member this.TryPrecompiledOutPath (sourceDir, relativePath) = None
                member this.GetOrAddDeduplicateTargetDir (importDir, addTargetDir) = ""
        }
        let projCracked = ProjectCracked.Init(cliArgs projFile options)
        let checker = InteractiveChecker.Create(projCracked.ProjectOptions)
        let fsharpAssemblies = checker.GetImportedAssemblies() |> Async.RunSynchronously
        let filePaths, sourceReader = Fable.Compiler.File.MakeSourceReader projCracked.SourceFiles
        let results = checker.ParseAndCheckProject(projCracked.ProjectFile, filePaths, sourceReader, Array.last projCracked.SourceFilePaths, ignore) |> Async.RunSynchronously

        let project = Project.From (projFile, projCracked.SourceFilePaths, results.AssemblyContents.ImplementationFiles, fsharpAssemblies)
        let fableLibDir = ""

        let cache =  Fone.Database.FableCompilationCache()
        Fable.C.Helpers.database.Value <- cache
        // let filePath = Array.last projCracked.SourceFilePaths
        let compiledFiles = [|
            for filePath in projCracked.SourceFilePaths do
                let com = CompilerImpl(filePath, project, options, fableLibDir)
                let file = Fable.Transforms.FSharp2Fable.Compiler.transformFile com
                let addMethod (decl: Fable.AST.Fable.MemberDecl) =
                    match decl.MemberRef with
                    | Fable.MemberRef(declaringEntity, info) ->
                        let ent = com.GetEntity declaringEntity
                        let mem = ent.TryFindMember info
                        cache.Update (Database.Parsing.AddMember (decl.MemberRef, file, ent, mem.Value, decl))
                    | _ -> ()
                for decl in file.Declarations do
                    match decl with
                    | Fable.AST.Fable.ClassDeclaration decl ->
                        cache.Update (Database.Parsing.AddEntity (file, com.GetEntity decl.Entity))
                        match decl.Constructor with
                        | Some ctor -> addMethod ctor
                        | None -> ()
                    | Fable.AST.Fable.MemberDeclaration decl ->
                        addMethod decl
                    | _ -> ()

                let transformedFile = Fable.Transforms.FableTransforms.transformFile com file
            // Fable.C.Helpers.database.Value <- {
            //     new Fable.C.Helpers.Type.ICompiler with
            //         member this.TryGetEntity entityRef = None
            //         member this.TryGetMember memberRef = None
            //         member this.GetEntity entityRef = Unchecked.defaultof<_>
            //         member this.GetMember memberRef = Unchecked.defaultof<_>
            // }
                let c_file = Fable.C.File.transformFile com transformedFile
                Fable.C.File.writeFile filePath c_file.includes c_file.compiledModule c_file.static_constructor
        |]
        let header =
            let com = CompilerImpl(projCracked.SourceFilePaths |> Seq.last, project, options, fableLibDir)
            Fable.C.ProjectWriter.writeModuleHeaderFile
                {
                    com = com
                    currentFile = "Program.fs"; idents = []
                    db = Fable.C.Helpers.database.contents
                    // todo:
                    file = Unchecked.defaultof<_>
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
        ()

[<EntryPoint>]
let main argv =
    let testsProjFile =
        // Path.Join(__SOURCE_DIRECTORY__, "../tests/C/Fable.Tests.C/Fable.Tests.C.fsproj")
        Path.Join(__SOURCE_DIRECTORY__, "../src/quicktest/Quicktest.fsproj")
        // "C:/Users/Dave/projects/Fable/src/quicktest/Quicktest.fsproj"
    // File.WriteAllText("/Quicktest.fsproj", projText)
    // let quicktest = Path.Join(__SOURCE_DIRECTORY__, "../tests/Fable.Tests.C/Fable.Tests.C.fsproj")
    // Compiler.compileSingleFile projFile // "C:/Users/Dave/projects/Fable/src/quicktest/QuickTest.fs"
    Compiler.compileProject testsProjFile
    0
