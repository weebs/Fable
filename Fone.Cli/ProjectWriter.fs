module rec Fable.C.ProjectWriter


open System.Text
open System.Threading
open System.Diagnostics

open System
open System.Collections

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms.FSharp2Fable

open Fable.C.AST
open Fable.C.C99Compiler

open Helpers
open ReplaceExpr
open Transforms
//open type print
open Fone.DataType

#if !FABLE_COMPILER
let launchProcess workingDirectory (name: string) (args: #seq<string>) =
    let start = ProcessStartInfo();
    //start.FileName <- "/usr/bin/python3";
    start.WorkingDirectory <- workingDirectory // io.path.join(workingDirectory, "build")
    start.FileName <- name
    start.Arguments <- String.Join(" ", args) //$"";
    start.UseShellExecute <- false
    start.RedirectStandardOutput <- false
    start.RedirectStandardError <- false
    // start.RedirectStandardInput <- false
    start
#endif

//let compilationResults: Map<string, FileCompilationResults> ref = ref Map.empty
//let genericClassDeclarations = ref Map.empty
//let classDeclarations: Map<string, ClassDecl> ref = ref Map.empty
//let memberDeclarations = ref Map.empty
//module Window =
//    open System
//    open Photino.Blazor
//    open Microsoft.Extensions.DependencyInjection
//    open Bolero.Html
//    type App() =
//        inherit Bolero.Component()
//
//        let mutable count = 0
//
//        override this.OnInitialized() =
//            ()
//
//        override this.Render() =
//            div {
//                span { text "Hello!" }
//                span { text (string count) }
//                button {
//                    on.click (fun _ -> count <- count + 1)
//                }
//            }
//    let appTask = ref None
//    let startApp () =
//        if appTask.Value = None then
//            appTask.Value <- Some <| Thread(ThreadStart(fun () ->
////                task {
//                    let app =
//                        let appBuilder = PhotinoBlazorAppBuilder.CreateDefault([||])
//
//                        appBuilder.Services
//                            .AddLogging()
//                    //        .AddBlazorWebView()
//                            |> ignore
//
//                        // register root component and selector
//                        appBuilder.RootComponents.Add<App>("app")
//
//                        let app = appBuilder.Build()
//
//                        // customize window
//                        app.MainWindow
//                            .SetLogVerbosity(0)
//                            .SetTitle("Photino.Bolero")
//                            |> ignore
//
//                        app
//                    app.Run())).Start()
////                }
//        appTask.Value


type Build = // todo: a lot of what i want is already available in some of the fable util module
    static member Let (name: string, value: Expr) body =
        Let({ Name = name; Type = value.Type; IsMutable = false; IsThisArgument = false; IsCompilerGenerated = false; Range = None },
            value, body)
    static member Value (value: obj) =
        match value with
        | :? string as s -> Value(ValueKind.StringConstant s, None)
        | _ -> Value(ValueKind.Null Any, None)
let test_expr () =
    Build.Let("foo", Build.Value("asdf")) <|
        Value(ValueKind.UnitConstant, None)

//let app = ref (Unchecked.defaultof<_>)
// let transformFsFile (_com: Fable.Compiler) (file: File) =
// //    app.Value <- Window.startApp ()
//     if _com.CurrentFile.Contains ".fsx" then
//         ()
//     else
//         realTransformFile _com file |> ignore

// let updateCompiler (_com: Fable.Compiler) =
//     compiler.UpdateFile(_com.CurrentFile, FileCompilationResults.Empty, ())
//     compiler.UpdateFile(_com.CurrentFile, FileCompilationResults.AddMemberDeclaration, (memberDecl, m, Unchecked.defaultof<_>))
//     compiler.UpdateFile(_com.CurrentFile, FileCompilationResults.AddClassDeclaration, classDecl)
//
//     // todo: includes
//     // compiler.UpdateFile(sourceFilePath, FileCompilationResults.AddInclude, i)
//
let createMainFunction (actualMain: string option) : C.FunctionInfo =
    {
        id = "main"
        return_type = C.Int
        args = [ ("argc", C.Int); ("args", C.Ptr (C.Ptr (C.Char))) ]
        body = [
            match actualMain with
            | Some main ->
                C.Emit "Runtime____start();"
                C.Declaration {
                    _type = C.Ptr C.Void
                    name = "ptr"
                    value = C.ExprAssignment <| C.Call("malloc", [ C.Expr.Emit "sizeof(args)" ])
                    requiresTracking = false
                }
                C.Declaration {
                    _type = C.Ptr <| C.Type.UserDefined ("System_Array__charptr", false, None)
                    name = "string_args"
                    value = C.ExprAssignment <| C.Call("System_Array__charptr_ctor", [ C.Expr.Emit "argc"; C.Expr.Emit "ptr" ])
                    requiresTracking = false
                }
            | _ -> ()

            C.Statement.Expression (C.Call ("____assembly_init", []))

            match actualMain with
            | Some main ->
                C.Statement.Expression (C.Call (main, [ C.Expr.Emit "string_args" ]))
            | _ -> ()
        ]
    }
let writeMainFile (projHeaderName: string) (actualMain: string option) =
    let main: C.FunctionInfo = createMainFunction actualMain

    $"// #include \"{projHeaderName}\"\n\n" +
    (Writer.writeFunction (SourceBuilder()) main)

let echoType (t: Type) = sprintf "| Fable.Type: %A" t

// let appendModuleIncludes (projectFile: string) (sb: StringBuilder) =
//     let buildDir =
//         IO.Path.GetDirectoryName(projectFile)
//     let projHeaderName =
//         IO.Path.GetFileNameWithoutExtension(projectFile) + ".h"
//     // sb.AppendLine($"#include \"{projHeaderName}\"") |> ignore
//     sb
//         .AppendLine("#include <stdint.h>")
//         .AppendLine("#include <stdio.h>")
//         .AppendLine("#include <stdbool.h>")
//         .AppendLine("#include <stdlib.h>")
//         .AppendLine("")
//     |> ignore
let writeModuleHeaderFile context (projPath: string) =
    let buildDir = IO.Path.GetDirectoryName(projPath)
    let projHeaderName = IO.Path.GetFileNameWithoutExtension(projPath) + ".h"
    let srcDir = Path.Combine(buildDir, "build")
    let path = Path.Combine (srcDir, projHeaderName)
    let def_name =
        IO.Path.GetFileNameWithoutExtension(projPath)
            .Replace(".", "_")
            .Replace("-", "_") + "_header"
    let sb = CompiledOutputBuilder()
    sb
        .AppendLine($"#pragma once")
        // .AppendLine($"#ifndef {def_name}")
        .AppendLine($"#define {def_name}") |> ignore
    let packagesToInclude = includedPackages.Value
    for package in packagesToInclude do
        sb.AppendLine($"#define __INCLUDE_PACKAGE_{package}__")
        |> ignore
    // todo: for NES
    // sb.AppendLine("\n#include \"neslib.h\"\n#define true 1\n#define false 0\n\n")
    // |> ignore
    sb
        .AppendLine("#include <stdint.h>")
        .AppendLine("#include <stdio.h>")
        .AppendLine("#include <stdbool.h>")
        .AppendLine("#include <stdlib.h>")
        .AppendLine("#include <string.h>")
        // .AppendLine("#include \"map.h\"")
    |> ignore
    sb.AppendLine "#define UNIT ((void*)0)" |> ignore
    for text in header_emit_texts.Value do
        sb.AppendLine($"{text}")
        |> ignore
    header_emit_texts.Value <- []
//    if projHeaderName <> "Fable.Library.C.h" then
//        let s: string = """
//EM_JS(void, evalPointer, (void* ptr), {
//    let s = '';
//    let index = ptr;
//    while (getValue(index, 'i8') != 0) {
//        s += String.fromCharCode(getValue(ptr, 'i8'))
//        index++;
//    }
//    eval(s);
//});"""
//        sb.AppendLine s |> ignore
//    if not (path.Contains("src/fable-library-c/")) then
//        sb.AppendLine("#include \"Fable.Library.C.h\"") |> ignore
    let extraIncludes = compiler.Files |> Seq.map compiler.GetResult |> Seq.map (fun r -> r.extraIncludes) |> Seq.collect id |> Seq.distinct
    for i in extraIncludes do
        if i.EndsWith ".js" then
            ()
        else
            let i = if i.StartsWith "<" && i.EndsWith ">" then i else $"\"{i}\""
            sb.AppendLine($"#include {i}")
            |> ignore
    sb
        //.AppendLine("void release(void* root);")
        //.AppendLine("void add_to_root(void* root, void** obj, void* finalizer);")
//        .AppendLine("struct TestC_Module_Root* init_root();")
//        .AppendLine("void assign_to_obj(void** location, void* newValue);")
        // .AppendLine("#ifndef __thread_context_static_var")
        // .AppendLine("#define __thread_context_static_var")
        // .AppendLine("_Thread_local static int __thread_context = 0;")
        // .AppendLine("#endif")
        .AppendLine("")
    |> ignore
    // sb.AppendLine("static thread_local int __thread_context = 0;")
    // todo: thread_local not working
    sb.AppendLine("static int __thread_context = 0;")
    |> ignore
    sb.AppendLine Runtime.code
    |> ignore
    let (generic_strings, generic_implementations) =
        Generic.generateGenericImplementations context path projHeaderName
    do
        // todo: Generic includes file or inclouded
        // appendModuleIncludes sb
        sb.AppendLine (generic_strings.ToString())
        |> ignore

    // Write static initializer functions (module do ... expressions)
    for (actionDecl, functionInfo) in State.initDeclarations.Value.Values do
        sb.AppendLine($"void {functionInfo.id}();") |> ignore
    sb.AppendLine("") |> ignore

    let methods_sb = CompiledOutputBuilder()

    // Write out the relevant header info for each file in the project
    for file in compiler.Files do
        Headers.writeFilesModuleHeaderStuffs methods_sb sb file

    methods_sb.Append "\n" |> ignore
    sb.Append (methods_sb.ToString()) |> ignore

    for (_, text) in generic_implementations do
        sb.AppendLine(text)
        |> ignore

    let init: C.FunctionInfo = {
        id = "____assembly_init"; args = []; return_type = C.Void; body = [
            for i in State.fileOrder.Value do
//                let i = initDeclarations.Value.[filename]
//            for i in (initDeclarations.Value.Keys |> Seq.rev) do
                C.Statement.Expression (C.Call (i, []))
        ]
    }
    sb.AppendLine("void ____assembly_init();") |> ignore
    // sb
    //     .AppendLine("#endif")
    |> ignore
    io.file.write(path, sb.ToString())
    let init_sb = StringBuilder()
    let module_include_name = IO.Path.GetFileName(path)
    // init_sb.AppendLine($"#include \"{module_include_name}\"\n") |> ignore
    init_sb.AppendLine(Writer.writeFunction (SourceBuilder ()) init) |> ignore
    io.file.write(path.Replace(module_include_name, "fable-init.c"), init_sb.ToString())
    let fable_init_c = io.files[path.Replace(module_include_name, "fable-init.c")]
    print.printfn $"{DateTime.Now.ToLongTimeString()} Finishing writing {module_include_name}"
    let file_output =
        let sb = CompiledOutputBuilder()
        io.files[path]
        + "\n\n"
        + (sb.ToString())
        // + "\n\n"
        // + "\n#endif"
        // + (generic_strings.ToString())
    // io.file.write(path, file_output)
    let sb = StringBuilder()
    let genericsFile = projHeaderName.Replace(".h", ".generics.implementation.fs.c")
    io.file.write(path.Replace(".h", ".generics.implementation.fs.c"), sb.ToString())
    let generics_file = io.files[path.Replace(".h", ".generics.implementation.fs.c")]
    print.printfn $"{DateTime.Now.ToLongTimeString()} Finishing writing generics.implementation.c"

    let project_intmain_file =
        let hasEntryPoint =
            compiler.Files
            |> Seq.map compiler.GetResult
            |> Seq.map (fun r -> r.memberDeclarations.Values)
            |> Seq.collect id
            |> Seq.exists (fst >> isEntryPoint database.contents)
        if not hasEntryPoint then
            sb.AppendLine("int main(int argc, char** argv);") |> ignore
            let s = writeMainFile projHeaderName None
            io.file.write(path.Replace(module_include_name, "project_main.c"), s)
            Some (io.files[path.Replace(module_include_name, "project_main.c")])
        else
            None

    let makefilePath = path.Replace(".h", ".sh")
    let files = System.String.Join(" ", compiler.Files |> Seq.map (fun f -> IO.Path.GetFileName(f.Replace(".fs", ".fs.c"))))

//    let files = files + $" Fable.Library.C.generics.implementation.fs.c {genericsFile} fable-init.c project_main.c"
    let files = files + $" {genericsFile} fable-init.c project_main.c"

    let outDir = IO.Path.GetDirectoryName(makefilePath)
    let fable_library_c_dir = "/home/dave/homebase/fable-c/fable-library-c"
    let packages =
        includedPackages.Value
        |> List.map (fun p -> $"$(pkg-config --cflags --libs {p})")
    let packages = String.Join(" ", packages)
    includedPackages.contents <- []

    // todo: #undef __cplusplus
    let makefileText =
        (if not (outDir.Contains("src/fable-library-c/")) then $"cp {fable_library_c_dir}/build/Fable.Library.C.h Fable.Library.C.h && " else "") +
        $"cp {fable_library_c_dir}/build/map.h . && " +
        $"cp {fable_library_c_dir}/build/*.fs.c . && " +
        $"cp {fable_library_c_dir}/build/*.debug.fs . && " +
        // todo: why does gcc not work with SDL2?
        // $"gcc $(pkg-config --cflags --libs sdl2) -g {files} -o a.out"
//        $"clang $(pkg-config --cflags --libs sdl2) $(pkg-config --cflags --libs ncurses) -g {files} -o a.out"
        $"clang -Wno-parentheses-equality {packages} -g {files} -o a.out && clear && ./a.out"
    io.file.write(makefilePath, makefileText)
    let wasm_makefileText =
        let emccOptions = System.String.Join(" ", [
            //"-pthread"
            "-sEXIT_RUNTIME=0"
            "-sALLOW_MEMORY_GROWTH"
            //"-sPROXY_TO_PTHREAD"
            //"-sASYNCIFY"
            //"-sPTHREAD_POOL_SIZE=12"
            //"-v"

            //"-fsanitize=address"

            "-Wno-parentheses-equality"
            "-g"
        ])
        (if not (outDir.Contains("src/fable-library-c/")) then $"cp {fable_library_c_dir}/build/Fable.Library.C.h Fable.Library.C.h && " else "") +
        $"cp {fable_library_c_dir}/build/map.h . && " +
        $"cp {fable_library_c_dir}/build/*.fs.c . && " +
        $"cp {fable_library_c_dir}/build/*.debug.fs . && " +
        // todo: why does gcc not work with SDL2?
        // $"gcc $(pkg-config --cflags --libs sdl2) -g {files} -o a.out"
        // $"~/apps/emscripten/emcc -pthread -sEXIT_RUNTIME -sPROXY_TO_PTHREAD -sASYNCIFY -sPTHREAD_POOL_SIZE=12 -sUSE_SDL=2 -sUSE_SDL_IMAGE=2 $(pkg-config --cflags --libs sdl2) -g {files} -o program.html"
//        $"~/apps/emscripten/emcc -sASYNCIFY -sUSE_SDL=2 -sUSE_SDL_IMAGE=2 $(pkg-config --cflags --libs sdl2) -g {files} -o program.html"
        $"~/apps/emscripten/emcc {emccOptions} {files} -o program.html"
    let wasm_path = makefilePath.Replace(".sh", ".wasm.sh")
    io.file.write(wasm_path, wasm_makefileText)
    // todo: only one build at a time?
    #if !FABLE_COMPILER
    if 1 <> 1 then
        async {
            log "Build starting!"
                // if Environment.GetEnvironmentVariable("FABLE_CC").Trim().ToLower() = "clang" then
                //     launchProcess "sh" [ makefilePath ]
                // else
            log $"Building wasm: {wasm_path}"
            let buildInfo =
                launchProcess (io.path.join(buildDir, "build/")) "sh" [ wasm_path ]
            let build = Process.Start(buildInfo)
            build.WaitForExit()
            log "Build finished"
            compiler.BroadcastFinishedCompilation()
        } |> Async.StartAsTask |> ignore
    #endif
    {| file = file_output; main = project_intmain_file; init = fable_init_c; generics = generics_file |}

let writeNesHeader context (projPath: string) =
    let result = writeModuleHeaderFile context projPath
    let file =
        result.file.Replace("#include <stdio.h>
    #include <stdbool.h>
    #include <stdlib.h>
    #include <string.h>", "#include \"neslib.h\"\n#define true 1\n#define false 0").Replace("____assembly_init();", "main();")
    {| result with file = file |}
