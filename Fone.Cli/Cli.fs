namespace Fone.Cli
open Fable
open System
open System.Reflection
open Fable.C
open Fable.C.C99Compiler
open Fone.Repl
open Fone
open Microsoft.FSharp.Reflection

module Fone =
    // let mutable Repl__db: Map<string, obj> = Map.empty

    type Msg =
        | File of Fable.Compiler * Fable.AST.Fable.File * Fable.C.Fable2C.CompiledAst
    let mutable runTask = (fun (f: unit -> unit) -> ())
    let mutable queue = Unchecked.defaultof<MailboxProcessor<Msg>> 
    let mutable compilerLock = obj()
    let mutable itemsQueue = []
    let qLock = obj()

    let database = Database.FableCompilationCache()

    // Environment.CurrentDirectory <- IO.Path.GetDirectoryName(args[2])
    let writeProjectHeader (mainFilePath: string) (projectFilePath: string) =
        let projName = IO.Path.GetFileNameWithoutExtension projectFilePath
        try
            let header = Fable2C.writeModuleHeaderFile { currentFile = mainFilePath } (projName + ".h")
            let file = projectFilePath.Replace(".fsproj", ".h")
            IO.File.WriteAllText (file, header.file)
            IO.File.WriteAllText (IO.Path.GetDirectoryName(projectFilePath) + "/generics.fs.c", $"#include \"{projName}.h\"\n" + header.generics)
            Ok file
        with e ->
            Error e
    let writeFile (projPath: string) (sourcePath: string) (output: string) =
        let projName = IO.Path.GetFileNameWithoutExtension(projPath)
        let outPath =
            let outPath = sourcePath + ".c"
            if outPath.Contains "fable-library-c/" then
                 IO.Path.GetDirectoryName(projPath) + "/fable_modules/fable-library-c/" + (IO.Path.GetFileName(outPath))
            else
                outPath
        if not (IO.Directory.Exists(IO.Path.GetDirectoryName(outPath))) then
            IO.Directory.CreateDirectory(IO.Path.GetDirectoryName(outPath))
            |> ignore
        let relativeHeaderFilePath = IO.Path.GetRelativePath(outPath, IO.Path.GetDirectoryName(projPath) + "/" + projName + ".h").Substring(3)
        try
            IO.File.WriteAllText (outPath, $"#include \"{relativeHeaderFilePath}\"\n" + output)
            printfn $"Wrote {sourcePath} to {outPath} (project: {projPath})"
            Ok outPath
        with e -> Error e
    let compileFile (compiler: Fable.Compiler) (file: Fable.AST.Fable.File) = async {
        printfn $"Received file: {compiler.CurrentFile}"
        printfn $"Source Files: %A{compiler.SourceFiles}"
        let compiledFiles = Repl__db["files"] :?> Map<string, Fable.Compiler * Fable.AST.Fable.File>
        let _mod = Fable2C.transformFile compiler file
        let output = Fable2C.writeModule compiler.CurrentFile _mod
        
        writeFile compiler.ProjectFile compiler.CurrentFile output
        |> printfn "%A"
        
        let files = io.files.Keys |> Array.ofSeq |> Array.filter (fun i -> i.Contains ".debug.fs")
        files |> Array.iter (fun file -> try IO.File.WriteAllText (file, io.files[file]) with ex -> ())
        // if (compiler.CurrentFile = (compiler.SourceFiles |> Array.last)) then
        let projName = IO.Path.GetFileNameWithoutExtension(compiler.ProjectFile)

        ()
        // if (compiledFiles.Count >= compiler.SourceFiles.Length) then
        //     let header = Fable2C.writeModuleHeaderFile { currentFile = compiler.CurrentFile } (projName + ".h")
        //     IO.File.WriteAllText (compiler.ProjectFile.Replace(".fsproj", ".h"), header.file)
        //     IO.File.WriteAllText (IO.Path.GetDirectoryName(compiler.ProjectFile) + "/generics.fs.c", $"#include \"{projName}.h\"\n" + header.generics)
    }
    let mutable receivedFiles = System.Collections.Concurrent.ConcurrentDictionary<string, _ * Fable.AST.Fable.File>()
    let compile (compiler: Fable.Compiler) (file: Fable.AST.Fable.File) = async {
        // Keep track of files between restarts
        if not (Repl__db.ContainsKey "files") then
            Repl__db <- Repl__db.Add("files", Map.ofList [ compiler.CurrentFile, (compiler, file) ])
        else
            Repl__db <- Repl__db.Add("files", (Repl__db["files"] :?> Map<string, Fable.Compiler * Fable.AST.Fable.File>).Add(compiler.CurrentFile, (compiler, file)))
            
        // let agent = MailboxProcessor.Start <| Parsing.fableDeclAgent database.Update compiler
        // file.Declarations |> List.iter (fun decl -> agent.Post (compiler.CurrentFile, file, decl))
        receivedFiles[compiler.CurrentFile] <- (compiler, file)
        let agent = Database.Parsing.logAgent
        let agentPost = Database.Parsing.logAgent.Post
        file.Declarations
            |> List.map (
                Database.Parsing.parseDecl
                    Database.Parsing.logAgent.Post
                        compiler compiler.CurrentFile file
            )
            |> List.collect id
            |> List.iter database.Update
        // let headerFileLock = obj()
        // let mutable writingOutput = false
        if receivedFiles.Count >= compiler.SourceFiles.Length then
            let queueSize = database.QueueSize
            let! itemsQueueEmpty = async {
                if queueSize = 0 then
                    do! Async.Sleep 1000
                return lock qLock <| fun () ->
                    itemsQueue.Length = 0
            }
            if itemsQueueEmpty && queueSize = 0 then
                // let continue =
                    // lock headerFileLock <| fun _ ->
                        // if not writingOutput then
                            // writingOutput <- true
                            // true
                        // else
                            // false
                // if continue then
                do
                    printfn "Compilation finished"
                    let compilationTasks =
                        receivedFiles.Values
                        |> Seq.map (fun (compiler, file) -> async {
                            try
                                return Some (compiler.CurrentFile, Fable2C.transformFile compiler file)
                            with ex ->
                                printfn $"{ex}"
                                return None
                        })
                        |> Async.Sequential
                    async {
                        let! result = compilationTasks
                        let result = result |> Array.filter Option.isSome |> Array.map Option.get |> Map.ofArray
                        for file in compiler.SourceFiles do
                            let astModule = result[file]
                        // for (file, astModule) in result do
                            printfn $"{file}: {astModule.compiledModule.contents.Count}"
                            let output = Fable2C.writeModule file astModule
                            printfn $"Writing output file {file}"
                            writeFile compiler.ProjectFile file output |> printfn "%A"
                            printfn "Compile file %s" compiler.CurrentFile
                            // if file.EndsWith "Program.fs" then
                            //     compiler.GetImplementationFile compiler.CurrentFile
                            //     |> Helpers.Print.printObj 0 |> printfn "%s"
                        let mainFile = compiler.SourceFiles |> Array.last
                        printfn "Writing header file"
                        Transforms.compiler.SetFiles (Array.toList <| Array.rev compiler.SourceFiles)
                        writeProjectHeader mainFile compiler.ProjectFile
                        |> printfn "%A"
                        // lock headerFileLock <| fun _ ->
                        //     writingOutput <- false
                    
                    } |> Async.Start
            else
                printfn "Queue: %d" queueSize
        // return! compileFile compiler file
        // let fableFile = Fable.Transforms.FSharp2Fable.Compiler.transformFile compiler
        // let file = Fable.Transforms.FableTransforms.transformFile compiler fableFile
        // printfn "%A" output
    }
    let mutable callback_loop = None
    Helpers.database.contents <- database

    let callback (compiler: Fable.Compiler) args resolver isSilent file =
        lock compilerLock <| fun () ->
            if callback_loop = None then
                callback_loop <- Some <| task {
                    while true do
                        let items =
                            lock qLock <| fun () ->
                                if itemsQueue.Length > 0 then
                                    let i = itemsQueue
                                    itemsQueue <- []
                                    i
                                else []
                        for (compiler, file) in items do
                            do! compile compiler file
                        do! Async.Sleep 500
                }
        printfn "Received callback from Fable.Cli.Main"
        try
            // let file =
            lock compilerLock <| fun _ ->
                let fableFile = Fable.Transforms.FSharp2Fable.Compiler.transformFile compiler
                let file = Fable.Transforms.FableTransforms.transformFile compiler fableFile
                lock qLock <| fun () -> itemsQueue <- (compiler, file) :: itemsQueue
                // file
            // async { do! compile compiler file }
        with error -> printfn $"%A{error}" //async { return () }
        async { () }
    
module Cli =
    open System

    let start args =
        let mutable stillRunning = true
        // try
        //     Repl__db <- Repl__db.Remove("fable_compiler")
        //     let t = Repl__db["fable_thread"] :?> Threading.Thread
        //     t.Interrupt()
        // with error ->
        //     printfn $"{error}"

        if not (Repl__db.ContainsKey "fable_compiler") then
            // System.Reflection.Assembly.LoadFile "/home/dave/repos/Fable/src/Fable.Cli/bin/Debug/net6.0/Fable.Compiler.Service.dll"
            // Assembly.LoadFile "/home/dave/repos/Fable/lib/fcs/FSharp.Compiler.Service.dll"
            // System.Reflection.Assembly.LoadFile "/home/dave/repos/Fable/src/Fable.Cli/bin/Debug/net6.0/fable.dll"
            Repl__db <- Repl__db.Add("fable_compiler", true)
            let t = 
                Threading.Thread(Threading.ThreadStart(fun () ->
                    try
                        let returnCode = Fable.Cli.Program.main args
                        printfn "Finished running, return code = %d" returnCode
                    with error ->
                        stillRunning <- false
                        printfn $"{error}"
                    Repl__db <- Repl__db.Remove("fable_compiler")
                ))
            Repl__db <- Repl__db.Add("fable_thread", t)
            t.Start()
        elif Repl__db.ContainsKey "files" then
            try
                let files = Repl__db["files"] :?> Map<string, Fable.Compiler * Fable.AST.Fable.File>
                let files = files |> Array.ofSeq
                for kv in files |> Seq.rev do
                    Fone.compile (fst kv.Value) (snd kv.Value)
                    |> Async.Start
            with e ->
                printfn $"{e}"
                // |> Async.RunSynchronously


    let makeFableArguments (projectPath: string) =
        Environment.CurrentDirectory <- IO.Path.GetDirectoryName (IO.Path.GetFullPath projectPath)
        [|
            "watch"
            "--cwd"
            projectPath
            "--lang"
            "fone"
            "--exclude"
            "Fable.Core"
            "--noCache"
        |]
    let env_variable_args = System.Environment.GetEnvironmentVariable("FSHARP_ARGS")
    let _args = 
        if env_variable_args <> null then env_variable_args.Split(' ')
        // elif Environment.GetCommandLineArgs().Length > 1 then
        //     Environment.GetCommandLineArgs()
        else [|
            "watch"; "--cwd"
            // "/home/dave/repos/Fable/tests/Fable.Tests.C/Fable.Tests.C.fsproj"
            // "/home/dave/scratch/asdf/asdf.fsproj"
            // "/home/dave/repos/Fable/src/fable-standalone/test/bench-compiler/fable.c/QuickTest/QuickTest.fsproj"
            "C:\\code\\Fable\\src\\fable-standalone\\test\\bench-compiler\\fable.c\\QuickTest\\QuickTest.fsproj"
            
            // "/home/dave/repos/Fable/Fone/Fone.fsproj"
            // "/home/dave/repos/fone/Samples/webview/web_view.fsproj"
            // "/home/dave/repos/fone/Fone.Repl/samples/raycast-fs/raycast-fs.fsproj"
            
            "--lang"; "c"
            "--noCache"
            "--exclude"; "Fable.Core"
        |]
        
    let fsi_args =
        let fsi_args =
            match _args |> Array.tryFindIndex (fun arg -> arg.EndsWith ".fsproj") with
            | Some i when env_variable_args = null ->
                printfn $"================================== Setting Environment.CurrentDirectory for project {_args[i]} =================================="
                Environment.CurrentDirectory <- IO.Path.GetDirectoryName(IO.Path.GetFullPath(_args.[i]))
                _args |> Array.updateAt i (IO.Path.GetFullPath(_args.[i]))
            | _ -> _args
        let project =
            IO.Path.Join(__SOURCE_DIRECTORY__, "Fone.fsproj")
        let commandLineArgs = Environment.GetCommandLineArgs()
        if env_variable_args = null &&
           commandLineArgs.Length > 2 &&
           IO.Path.GetFullPath(commandLineArgs[1]) = project then
            commandLineArgs |> Array.skip 2
        else
            fsi_args
    #if !INTERACTIVE
    [<EntryPoint>]
    let main args =
        let args =
            if args.Length = 0 then
                [|
                    // "/home/dave/repos/Fable/src/fable-standalone/test/bench-compiler/fable.c/QuickTest.Shaders/QuickTest.Shaders.fsproj"
                    "C:\\code\\Fable\\src\\fable-standalone\\test\\bench-compiler\\fable.c\\QuickTest\\QuickTest.fsproj"
                |]
            else args
        Fable.Cli.Pipeline.Fone.overrideCompile <- Some Fone.callback
        printfn $"main args are %A{args}"
        Environment.SetEnvironmentVariable("DOTNET_USE_POLLING_FILE_WATCHER", "1")
        let args = if args.Length = 1 then makeFableArguments args[0] else args
        
        start args; 0
    #else
    Environment.SetEnvironmentVariable("FSHARP_INTERACTIVE_HOST", "true")
    Fable.Cli.Pipeline.Fone.overrideCompile <- Some Fone.callback
    start fsi_args
    //Repl__hostComponent (Bolero.Html.div { "howdy" })
    #endif
