module Fone.Cli.Program
open Cli
open System.IO


[<EntryPoint>]
let main argv =
    let testsProjFile =
        Path.Join(__SOURCE_DIRECTORY__, "../tests/C/Fable.Tests.C/Fable.Tests.C.fsproj")
        // Path.Join(__SOURCE_DIRECTORY__, "../src/quicktest/Quicktest.fsproj")
        // "C:/Users/Dave/projects/Fable/src/quicktest/Quicktest.fsproj"
    // File.WriteAllText("/Quicktest.fsproj", projText)
    // let quicktest = Path.Join(__SOURCE_DIRECTORY__, "../tests/Fable.Tests.C/Fable.Tests.C.fsproj")
    // Compiler.compileSingleFile projFile // "C:/Users/Dave/projects/Fable/src/quicktest/QuickTest.fs"
    Compiler.compileProject testsProjFile
    0

