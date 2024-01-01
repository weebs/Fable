module rec Fable.C.Fable2C

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

type CompiledAst = { includes: string list; compiledModule: Map<string, C.ModuleDeclaration> ref; static_constructor: (ActionDecl * C.Statement list) list }
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
let writeNesModule (fileName: string) (_module: CompiledAst) : string =
    writeModule fileName _module
    |> fun text -> text.Replace("
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>", "")
let writeModule (fileName: string) (_module: CompiledAst) : string =
    writeFile fileName _module.includes _module.compiledModule _module.static_constructor
let writeFile sourceFilePath (includes: string list) (compiledModule: Map<string, C.ModuleDeclaration> ref) (static_constructor: (ActionDecl * C.Statement list) list) =
    // todo: what was the point of the fiileName arg?
    let fileName = IO.Path.GetFileName(sourceFilePath)
    let _com = database.contents
    let testPath =
        let dir = Path.Combine(Path.GetDirectoryName(sourceFilePath), "build")
        let file = Path.Combine(dir, Path.GetFileName(sourceFilePath)).Replace(".fs", ".fs.c")
        file
    let sb = StringBuilder()
    
    // todo: conditional
    // if conditional_packages.contents.ContainsKey(context.currentFile) then
    //     sb.AppendLine($"#ifdef __INCLUDE_PACKAGE_{conditional_packages.contents.[context.currentFile]}__")
    //     |> ignore
    // #include
    // appendModuleIncludes sb
    
    let headers = Fable.C.Transforms.includedHeaders.Value
    includedHeaders.Value <- []
//    for header in headers do
//        sb.AppendLine $"#include \"{header}\"" |> ignore
    for i in (includes @ headers) do
        State.extraIncludes.Value <- Set.add i State.extraIncludes.Value
        compiler.UpdateFile(sourceFilePath, FileCompilationResults.AddInclude, i)

    // Write file local variables, type definitions, and lambda function declarations
    for item in compiledModule.Value do
        match item.Value with
        | C.StaticVar declarationInfo ->
            match declarationInfo.value with
            | C.ExprAssignment expr ->
                sb.AppendLine($"static {declarationInfo._type.ToTypeString()} {declarationInfo.name} = {Compiler.writeExpression expr};")
                |> ignore
            | C.StatementAssignment statement ->
                sb.AppendLine($"static {declarationInfo._type.ToTypeString()} {declarationInfo.name};")
                |> ignore
                // todo: build get/set functions
                // todo: collect static variables and make an initialize_module function that gets called by main
            | C.Default -> ()
        | C.Function functionInfo when functionInfo.id.Contains $"_anonymous_fn_" ->
            sb.AppendLine($"{Print.compiledFunctionSignature functionInfo};") |> ignore
        // todo: Extern gets in the way when using header files
        | C.Extern (name, args, return_type) ->
            // sb.AppendLine "#ifdef __cplusplus\nextern \"C\" {\n#endif"
            // |> ignore
            // sb.AppendLine($"extern {Print.compiledExternFunctionSignature (name, args, return_type) }")
            // |> ignore
            // sb.AppendLine "#ifdef __cplusplus\n}\n#endif"
            // |> ignore
            ()
        | _ -> ()
//            sb.Append(Compiler.write)
    sb.AppendLine("") |> ignore

    // Write file local function implementations
    for item in compiledModule.Value do
        match item.Value with
        | C.Function functionInfo ->
            let s = Compiler.writeFunction (SourceBuilder()) functionInfo
            sb.Append(s).AppendLine("") |> ignore
        | _ -> ()

    // Create an init function for the module if there's any action/module let value declarations
    if static_constructor.Length > 0 then
        let init_name = fileName.Replace(".fs", "_fs")
//            context.currentFile.Replace(IO.Path.GetDirectoryName(database.contents.ProjectFile), "").Replace("/", "").Replace("\\", "").Replace(".", "_");
        let init: C.FunctionInfo = {
            id = init_name
            return_type = C.Void
            args = []
            body = static_constructor |> List.map snd |> List.collect id
        }
        let module_constructor = Compiler.writeFunction (SourceBuilder()) init
        sb.AppendLine(module_constructor) |> ignore
        State.initDeclarations += (init_name, ((static_constructor |> List.map fst), init))
        if (not (List.contains init_name State.fileOrder.Value)) then
            State.fileOrder.Value <- State.fileOrder.Value @ [ init_name ]
            
    // todo: conditional includes
    // if conditional_packages.contents.ContainsKey(context.currentFile) then
    //     sb.AppendLine($"#endif")
    //     |> ignore
    log $"Writing file {sourceFilePath} to destination {testPath}"
    io.file.write(testPath, sb.ToString())
    sb.ToString() + "\n"
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
                }
                C.Declaration {
                    _type = C.Ptr <| C.Type.UserDefined ("System_Array__charptr", false, None)
                    name = "string_args"
                    value = C.ExprAssignment <| C.Call("System_Array__charptr_ctor", [ C.Expr.Emit "argc"; C.Expr.Emit "ptr" ])
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
    (Compiler.writeFunction (SourceBuilder()) main)

let echoType (t: Type) = sprintf "| Fable.Type: %A" t

let appendModuleIncludes (projectFile: string) (sb: StringBuilder) =
    let buildDir =
        IO.Path.GetDirectoryName(projectFile)
    let projHeaderName =
        IO.Path.GetFileNameWithoutExtension(projectFile) + ".h"
    // sb.AppendLine($"#include \"{projHeaderName}\"") |> ignore
    sb
        .AppendLine("#include <stdio.h>")
        .AppendLine("#include <stdbool.h>")
        .AppendLine("#include <stdlib.h>")
        .AppendLine("")
    |> ignore
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
        .AppendLine("#include <stdio.h>")
        .AppendLine("#include <stdbool.h>")
        .AppendLine("#include <stdlib.h>")
        .AppendLine("#include <string.h>")
        // .AppendLine("#include \"map.h\"")
    |> ignore
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

    // Write static initializer functions (module do ... expressions)
    for (actionDecl, functionInfo) in State.initDeclarations.Value.Values do
        sb.AppendLine($"void {functionInfo.id}();") |> ignore
    sb.AppendLine("") |> ignore

    // Generate #defi
    let methods_sb = CompiledOutputBuilder()

    // Write out the relevant header info for each file in the project
    for file in compiler.Files do
        Headers.writeFilesModuleHeaderStuffs methods_sb sb file

    methods_sb.Append "\n" |> ignore
    sb.Append (methods_sb.ToString()) |> ignore

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
    init_sb.AppendLine(Compiler.writeFunction (SourceBuilder ()) init) |> ignore
    io.file.write(path.Replace(module_include_name, "fable-init.c"), init_sb.ToString())
    let fable_init_c = io.files[path.Replace(module_include_name, "fable-init.c")]
    print.printfn $"{DateTime.Now.ToLongTimeString()} Finishing writing {module_include_name}"
    let (generic_strings, generic_implementations) = Generic.generateGenericImplementations context path projHeaderName
    let file_output =
        let sb = CompiledOutputBuilder()
        sb.AppendLine (generic_strings.ToString())
        |> ignore
        // for (_, text) in generic_implementations do
        //     sb.AppendLine text
        //     |> ignore
        io.files[path]
        + "\n\n"
        + (sb.ToString())
        // + "\n\n"
        // + "\n#endif"
        // + (generic_strings.ToString())
    // io.file.write(path, file_output)
    let sb = StringBuilder()
    // todo: Generic includes file or inclouded
    // appendModuleIncludes sb
    for (type_sig, text) in generic_implementations do
        // sb.AppendLine(type_sig) |> ignore
        let text: string = text
        sb.AppendLine(text)
        |> ignore
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

//    let projName = IO.Path.GetFileNameWithoutExtension(database.contents.ProjectFile).Replace(".", "_")
//    let headerName = projName + ".h"
//    let makefileName = projName + ".sh"
//    let makefilePath = path.Replace(".h", makefileName)
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

//        for decl in file.Declarations do
//            match decl with
//            | MemberDeclaration memberDecl -> print.printfn $"{memberDecl.Name}"; Display.fableExpression 1 memberDecl.Body |> Seq.iter Console.Write
//            | ClassDeclaration classDecl ->
//                match classDecl.Constructor with
//                | Some constructor -> print.printfn $"{classDecl.Entity.DisplayName} (constructor)"; Display.fableExpression 1 constructor.Body |> Seq.iter Console.Write
//                | _ -> ()
//            | _ -> ()
//        print.printfn "foo"
//    for kv in genericMethodDeclarations.Value do
//        let _member = database.contents.GetMember(kv.Value.MemberRef)
//        printf $"{kv.Key}:\t"
//    for kv in genericClassDeclarations.Value do
//        printf $"{kv.Key}:\t{kv.Value.Entity.SourcePath.Value}\t"
let transformFile (_com: Fable.Compiler) (file: File) =
    // database.contents <- _com
    print.printfn $"Database: %A{database.contents}"
    compiler.UpdateFile(_com.CurrentFile, FileCompilationResults.Empty, ())
    let context = { currentFile = _com.CurrentFile }
    do
        let dir = IO.Path.GetDirectoryName(_com.CurrentFile)
        let filename = IO.Path.GetFileName(_com.CurrentFile)
        let path = io.path.join(dir, $"build/{filename}.debug.fs")
        #if !FABLE_COMPILER
        // todo: no io in transforms!
        // if not (IO.Directory.Exists(io.path.join(IO.Path.GetDirectoryName(_com.ProjectFile), "build"))) then
        //     IO.Directory.CreateDirectory(io.path.join(IO.Path.GetDirectoryName(_com.ProjectFile), "build"))
        //     |> ignore
        #endif
        if io.file.Exists path then
            io.file.Delete(path)
//        IO.File.AppendAllText(path, $"{name}:\n{result}\n\n")
    print.printfn $"Transforming file: {_com.CurrentFile}"
    let compiledModule = ref Map.empty
    let mutable includes = []
    let mutable static_constructor: (ActionDecl * C.Statement list) list = []
    let rec loop decl =
        match decl with
        | ModuleDeclaration moduleDecl -> List.map loop moduleDecl.Members |> List.collect id | decl -> [ decl ]
    let declarations = file.Declarations |> List.map loop |> List.collect id
    for decl in declarations do
        match decl with
        | MemberDeclaration memberDecl when memberDecl.Args.Length = 0 ->
            let value =
                match memberDecl.Body with
                | Call(IdentExpr ident, _, _, _)
                | IdentExpr ident ->
                    C.StatementAssignment (C.Block (transformMember context [] memberDecl.Body))
                | _ ->
                    if Query.isSimpleExpr memberDecl.Body then C.ExprAssignment (transformExpr context [] memberDecl.Body)
                    else C.StatementAssignment (C.Block (transformMember context [] memberDecl.Body))
            State.addModuleStaticVar _com.CurrentFile (memberDecl.Name, memberDecl)
            compiledModule += (memberDecl.Name, C.StaticVar { _type = transformType [] memberDecl.Body.Type; name = memberDecl.Name; value = value })
            match memberDecl.MemberRef with
            | MemberRef (ent, info) ->
                try
                    compiler.AddMember (database.contents.GetMember(memberDecl.MemberRef))
                with ex -> printfn $"{ex}"
                let name = Print.compiledMethodName (memberDecl, [], ent)
                compiledModule += (name, C.Function {
                    id = name; args = []; return_type = transformType [] memberDecl.Body.Type; body = [
                        C.Return (C.Expr.Emit $"{memberDecl.Name}")
                    ]
                })
            | _ -> ()
//            memberDeclarations += ((_com.CurrentFile, memberDecl.Name), memberDecl)
            match database.contents.TryGetMember(memberDecl.MemberRef) with
            | Some m -> compiler.UpdateFile(context.currentFile, FileCompilationResults.AddMemberDeclaration, (memberDecl, m, Unchecked.defaultof<_>))
            | None ->
                for i in 1..10 do print.printfn "========================================"
                print.printfn $"Could not find member for ref: %A{memberDecl.MemberRef}"
                for i in 1..10 do print.printfn "========================================"
        | ActionDeclaration actionDecl ->
            match actionDecl.Body with
            | Emit(emitInfo, ``type``, sourceLocationOption) ->
                match emitInfo.Macro with
                | "____set_header_import" -> ()
                | _ -> transformExpr context [] actionDecl.Body |> ignore
            | _ ->
                let toCompile = Function.gatherAnonymousFunctions context actionDecl.Body
                for fn in toCompile do
                    compiledModule += fn
                static_constructor <- static_constructor @ [ (actionDecl, (transformMember context [] actionDecl.Body)) ]
        // | MemberDeclaration memberDecl when isVoidMain memberDecl file.Declarations ->
        //     ()
        | MemberDeclaration memberDecl ->
            let compiledName, isImport =
                match memberDecl.MemberRef with
                | MemberRef(ent, info) ->
                    try
                        compiler.AddMember (database.contents.GetMember(memberDecl.MemberRef))
                        // todo: if an Ident is not in the memberDecl.Body.UsedNames, i think that means it's a module var
                        let _member = database.contents.GetMember(memberDecl.MemberRef)
                        let attributes = _member.Attributes |> Seq.map (fun attribute -> attribute, database.contents.TryGetEntity(attribute.Entity)) |> Array.ofSeq
                        let isImport = _member.Attributes |> Seq.exists (fun attr -> attr.Entity.FullName = "System.Runtime.InteropServices.DllImportAttribute")
                        let fullName = _member.DeclaringEntity.Value.FullName
                        if isImport then
                            log $"================= dllImport {fullName} {memberDecl.Name} ===================="
                        for attribute in _member.Attributes do
                            log $"Attribute: %A{attribute.ConstructorArgs}\n"
                        Print.compiledMethodName (memberDecl, [], _member.DeclaringEntity.Value), isImport
                    with ex ->
                        print.printfn $"{ex}"
                        Print.compiledMethodName(memberDecl, [], ent), false
                | _ -> memberDecl.Name + "/* TODO MEMBERDECL NAME /*", false

            if isImport then
                // https://wiki.tcl-lang.org/page/Xector
                // todo: how to add the external declaration to the header
                // todo: or declare extern type?
                
                let return_type = transformType [] memberDecl.Body.Type
                let args = memberDecl.Args |> List.map (fun arg -> arg.Name, transformType [] arg.Type)
                compiledModule += (compiledName, C.Extern (memberDecl.Name, args, return_type))

                // todo: disabled this for now
                if not isImport then
                    compiledModule += (compiledName, C.Function {
                        id = compiledName
                        return_type = return_type
                        args = args
                        body = []
                    })
            else
                if Query.isGenericDecl memberDecl then
                    match memberDecl.MemberRef with
                    | MemberRef(declaringEntity, memberRefInfo) ->
                        try
                            let m = database.contents.GetMember(memberDecl.MemberRef)
                            let accurateFullName =
                                if m.GenericParameters.Length > 0 && not (declaringEntity.FullName.Contains "`") then
                                    declaringEntity.FullName + $"`{m.GenericParameters.Length}"
                                else
                                    declaringEntity.FullName
                            State.genericMethodDeclarations := State.genericMethodDeclarations.contents.Add ((accurateFullName, memberDecl.Name), memberDecl)
                            State.genericMethodDeclarations := State.genericMethodDeclarations.contents.Add ((declaringEntity.FullName, memberDecl.Name), memberDecl)
                        with ex -> printfn $"{ex}"
                    | GeneratedMemberRef generatedMember ->
                        ()
                else
//                    memberDeclarations += ((fullName, memberDecl.Name), memberDecl)
                    #if !FABLE_COMPILER
                    if displayExpressions then
                        let result = Display.memberDeclaration database.contents memberDecl
                        ()
                    #endif
//                    let compiledName = Print.compiledMethodName (memberDecl, [], database.contents.GetMember(memberDecl.MemberRef).DeclaringEntity.Value)
                    includes <- Query.getIncludes memberDecl.Body @ includes
                    let id = compiledName // if compiledName.EndsWith("main") then "main" else compiledName
                    print.printfn $"Transforming function {id}"
//                    print.printfn $"{Compiler.writeFunction sb f}"

                    let f = Function.transformFunc context id memberDecl.Args memberDecl.Body []
                    match database.contents.TryGetMember(memberDecl.MemberRef) with
                    | Some m -> compiler.UpdateFile(context.currentFile, FileCompilationResults.AddMemberDeclaration, (memberDecl, m, f))
                    | None ->
                        for i in 1..10 do print.printfn "========================================"
                        print.printfn $"Could not find member for ref: %A{memberDecl.MemberRef}"
                        for i in 1..10 do print.printfn "========================================"

                    compiledModule += (id, C.Function f)
                    let toCompile = Function.gatherAnonymousFunctions context memberDecl.Body
                    for fn in toCompile do
                        compiledModule += fn
        | ClassDeclaration classDecl ->
            let isGeneric = classDecl.Name.Contains "$"
            match database.contents.TryGetEntity(classDecl.Entity) with
            | Some ent ->
                compiler.AddEntity(ent)
                if not isGeneric then
                    // Todo we ask for this twice
                    let emitTypeAttr = ent.Attributes |> Seq.tryFind (fun attribute -> attribute.Entity.FullName.Contains("EmitType")) // = Const.emitType)
                    if emitTypeAttr = None then
                        compiler.UpdateFile(context.currentFile, FileCompilationResults.AddClassDeclaration, classDecl)
                    // if ent.IsFSharpUnion then
                        // let makeCaseStruct (case: UnionCase) =
                        //     let fields = case.UnionCaseFields
                        //     ()
                        // let makeUnion (ent: Entity) =
                        //     let cases = ent.UnionCases
                        //     ()
                        // makeUnion ent
                    if not ent.IsFSharpUnion then
                        let (_includes, declarations) = Function.Type.transformDeclaration context database.contents [] classDecl
                        includes <- _includes @ includes
                        for kv in declarations do
                            let emitTypeAttr = ent.Attributes |> Seq.tryFind (fun attribute -> attribute.Entity.FullName.Contains("EmitType")) // = Const.emitType)
                            if emitTypeAttr = None then
                                compiledModule += (kv.Key, kv.Value)
                else
                    compiler.AddGenericClassDecl classDecl
                    match classDecl.Constructor with
                    | Some constructor ->
                        // let m = database.contents.GetMember(constructor.MemberRef)
                        match database.contents.TryGetMember(constructor.MemberRef) with
                        | Some m -> compiler.AddMember(m)
                        | _ -> ()
                        State.genericMethodDeclarations := State.genericMethodDeclarations.contents.Add ((classDecl.Entity.FullName, constructor.Name), constructor)
    //                    compiler.AddMember()
                    | None ->
                        printfn "TODO: Constructors for generic record types"
                        ()
            | _ ->
                ()
//            classDeclarations := classDeclarations.contents.Add (classDecl.Entity.DisplayName, {| decl = classDecl; ent = com.GetEntity(classDecl.Entity) |})
        | _ ->
            printfn $"Transforming Declaration %A{decl}"

    print.printfn $"Compiled Module Size: {compiledModule.Value.Count}"
    writeFile _com.CurrentFile includes compiledModule static_constructor |> ignore
    { includes = includes; compiledModule = compiledModule; static_constructor = static_constructor }

        // todo: get rid of this example or rename the files?
//    let path = _com.CurrentFile.Replace(".fs", ".fs.c")
//    let headerPath = _com.CurrentFile.Replace(".fs", ".h")
//    let text = Compiler.compileFile (database.contents) {| compiled_module = compiledModule.Value; includes = includes |> List.distinct |}
//    System.IO.File.WriteAllText(path, text)
//    System.IO.File.WriteAllText(headerPath, text)
