open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open Fable.Cli.Main
open Fable.Transforms.State

let sampleProject = """
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

let options = Fone.Cli.Cli.Compiler.options
let runWithFableStuff () =
    let projCracked = ProjectCracked.Init(Fone.Cli.Cli.Compiler.cliArgs "program.fsproj" options)
    let checker = InteractiveChecker.Create(projCracked.ProjectOptions)
    let fsharpAssemblies = checker.GetImportedAssemblies() |> Async.RunSynchronously
    let filePaths, sourceReader = Fable.Compiler.File.MakeSourceReader projCracked.SourceFiles
    let results = checker.ParseAndCheckProject(projCracked.ProjectFile, filePaths, sourceReader, Array.last projCracked.SourceFilePaths, ignore) |> Async.RunSynchronously
    results

let transpileFile (code: string) (c: FSharpChecker) = task {
    // File.WriteAllText("main.fsx", simpleMain)
    let! opts, diagnostics = c.GetProjectOptionsFromScript("main.fsx", SourceText.ofString code, assumeDotNetFramework=false)
    let parsingOpts, parsingDiagnostics = c.GetParsingOptionsFromProjectOptions opts
    printfn $"%A{diagnostics}"
    let! fileResults = c.ParseFile ("main.fsx", SourceText.ofString code, parsingOpts)
    let! checkedProj = c.ParseAndCheckProject opts
    let! main, answer = c.ParseAndCheckFileInProject("main.fsx", 0, SourceText.ofString code, opts)
    let fileResults =
        match answer with
        | FSharpCheckFileAnswer.Succeeded checkFileResults -> checkFileResults.ImplementationFile.Value
        | FSharpCheckFileAnswer.Aborted -> failwith "todo"
    // let proj = Project.From("program.fsproj", [| "main.fsx" |], checkedProj.AssemblyContents.ImplementationFiles, [])
    let proj = Project.From("program.fsproj", [| "main.fsx" |], [ fileResults ], [])
    let fableLibDir = ""
    let com = CompilerImpl("main.fsx", proj, options, fableLibDir)

    let file = Fable.Transforms.FSharp2Fable.Compiler.transformFile com
    let transformedFile = Fable.Transforms.FableTransforms.transformFile com file

    let cache = Fone.Database.FableCompilationCache()
    cache.UpdateWithFile com file
    let c_file = Fable.C.File.transformFile false cache com transformedFile
    let output = Fable.C.File.writeFile "main.fs" c_file.includes c_file.compiledModule c_file.static_constructor
    return output
}
let fileText = "\
type Foo =
   { a: int; b: string }
let main () =
    let foo = { a = 1234; b = \"howdy !\" }
    printfn \"hello, world !\""
let code = "
let usesOption () =
    let o = Some \"1234\"
    match o with
    | Some n -> printfn $\"{n}\"
    | None -> printfn \"None\"
let main () = 0"

let foo = Module.M(1, 2)
let c = FSharpChecker.Create(keepAssemblyContents=true)
let t = transpileFile code c
let output = t.Result
printfn $"{output}"
// let
