module Fable.Cli.Api

// open FSharp.Compiler.SourceCodeServices
open System.IO
open FSharp.Compiler.SourceCodeServices
open Fable
open Fable.Cli.Main
open Fable.Compiler.Util
open Fable.Transforms.State

module Compiler =
    let compileSingleFile (filePath: string) = //async {
        let options: Fable.CompilerOptions =
            {
                TypedArrays = true
                ClampByteArrays = false
                Language = Language.Plugin
                Define = []
                DebugMode = true
                OptimizeFSharpAst = false // todo: TRUE
                Verbosity = Verbosity.Normal
                FileExtension = ".c"
                TriggeredByDependency = false
                NoReflection = true // todo: false
            }

        let projFile =
            "C:/Users/Dave/projects/Fable/src/quicktest/Quicktest.fsproj"

        let isSilent = false

        let cliArgs: CliArgs =
            {
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

        let pathResolver =
            { new PathResolver with
                member this.TryPrecompiledOutPath(sourceDir, relativePath) =
                    None

                member this.GetOrAddDeduplicateTargetDir
                    (
                        importDir,
                        addTargetDir
                    )
                    =
                    ""
            }

        let projCracked = ProjectCracked.Init(cliArgs)
        let checker = InteractiveChecker.Create(projCracked.ProjectOptions)

        let fsharpAssemblies =
            checker.GetImportedAssemblies() |> Async.RunSynchronously

        let filePaths, sourceReader =
            Fable.Compiler.File.MakeSourceReader projCracked.SourceFiles

        let results =
            checker.ParseAndCheckProject(
                projCracked.ProjectFile,
                filePaths,
                sourceReader,
                Array.last projCracked.SourceFilePaths,
                ignore
            )
            |> Async.RunSynchronously

        let project =
            Project.From(
                projFile,
                projCracked.SourceFilePaths,
                results.AssemblyContents.ImplementationFiles,
                fsharpAssemblies
            )

        let fableLibDir = ""
        let com = CompilerImpl(filePath, project, options, fableLibDir)
        let file = Fable.Transforms.FSharp2Fable.Compiler.transformFile com

        let transformedFile =
            Fable.Transforms.FableTransforms.transformFile com file

        let outPath = ""
        ()
// do! Pipeline.compileFile com cliArgs pathResolver isSilent outPath
// }
