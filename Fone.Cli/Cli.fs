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
    let runtime = """
typedef struct ref {
    void* data;
    int context;
    void* f;
    bool checked;
} ref;
typedef struct Runtime_pool {
    int size;
    int n;
    ref* data;
} Runtime_pool;

static Runtime_pool pool = { .size = 0, .n = 0, .data = 0 };

void Runtime_pool_track (void* addr, void* f) {
    (*(unsigned char*)addr)++;
    if (pool.n >= pool.size) {
        pool.size = pool.size == 0 ? 1 : pool.size * 2;
        ref* copy = pool.data;
        //int* copyctx = pool.context_data;
        pool.data = malloc(sizeof(ref) * pool.size);
        //pool.context_data = malloc(sizeof(int) * pool.size);
        for (int i = 0; i < pool.n; i++) {
            pool.data[i] = copy[i];
            //pool.context_data[i] = copyctx[i];
        }
        free(copy);
    }
    ref value = { .data = addr, .context = __thread_context + 1, .f = f, .checked = false };
    pool.data[pool.n] = value;
    //pool.context_data[pool.n] = __thread_context;
    pool.n++;
}

void Runtime_clear_pool() {
    int free_count = 0;
    for (int i = 0; i < pool.n; i++) {
        ref r = pool.data[i];
        if (r.checked || r.data == 0) {
            free_count++;
            continue;
        }
        //printf("Checking address %p\n", r.data);
        if (r.checked == false && r.context > __thread_context) {
            unsigned char* p = r.data;
            *p = *p - 1;
            pool.data[i].checked = true;
        }
        unsigned char count = *(unsigned char *)r.data;
        // todo: r.checked == true ?
        if (count <= 0) {
            void (*f)(void*) = r.f;
            //printf("Autorelease freeing %p\n", r.data);
            f(r.data);
            free_count++;
            pool.data[i].data = 0;
        }
    }
    // todo: Only free when pool is empty
    if (free_count == pool.n) {
        pool.size = 0;
        pool.n = 0;
        free(pool.data);
        pool.data = 0;
    }
}
void Runtime_pool_end() {
    // todo: Run destructors ?
    pool.size = 0;
    pool.n = 0;
    free(pool.data);
    pool.data = 0;
}

void* Runtime_autorelease(void* ptr, void* destructor) {
    //printf("Autorelease %p\n", ptr);
    unsigned char* p = ptr;
    *p = *p - 1;
    //void (*f)(void*) = destructor;
    // todo: Only track when *p > 0 ?
    Runtime_pool_track(ptr, destructor);
    //if (*p == 0) {
        //f(ptr);
    //}
    return ptr;
}
void Runtime_end_var_scope(void* ptr, void* destructor) {
    unsigned char* p = ptr;
    void (*f)(void*) = destructor;
    *p = *p - 1;
    if (*p <= 0) {{
        //printf("Freeing %p\n", ptr);
        f(ptr);
    }}
}
void Runtime_swap_value(void** location, void* value, void* destructor) {
    if (*location == value) { return; }
    unsigned char* p = value;
    *p = *p + 1;
    void* oldValue = *location;
    *location = value;
    if (oldValue != NULL)
        Runtime_end_var_scope(oldValue, destructor);
}
"""
    let options: Fable.CompilerOptions = {
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
        let header = Fable.C.Writer.writeModuleHeaderFile runtime { currentFile = ""; idents = [] } "/build/project.json"
        let files = io.files
        let generics = files |> Seq.find (fun kv -> kv.Key.Contains ".generics.")
        let output = compiledFiles |> String.concat "\n"
        let compiledOutput =
            $"{header.file}\n{header.generics}\n{output}"
            |> _.Replace("\r\n", "\n").Replace("\r", "")
        let outputPath = projFile.Replace (".fsproj", ".fs.c")
        File.WriteAllText (outputPath, compiledOutput)
        ()

[<EntryPoint>]
let main argv =
    let projText = """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <RollForward>Major</RollForward>
    <LangVersion>Preview</LangVersion>
  </PropertyGroup>
  <ItemGroup>
<!--    <Compile Include="Generics.fs" />-->
<!--    <Compile Include="QuickTest.fs" />-->
    <Compile Include="Main.fs" />
    <Content Include="quicktest.fs.js" />
    <Content Include="*.fs.h" />
    <Content Include="*.fs.c" />
  </ItemGroup>
  <ItemGroup>
    <ProjectReference Include="../Fable.Core/Fable.Core.fsproj" />
  </ItemGroup>
</Project>
"""
    let testsProjFile =
        // Path.Join(__SOURCE_DIRECTORY__, "../tests/Fable.Tests.C/Fable.Tests.C.fsproj")
        Path.Join(__SOURCE_DIRECTORY__, "../src/quicktest/Quicktest.fsproj")
        // "C:/Users/Dave/projects/Fable/src/quicktest/Quicktest.fsproj"
    // File.WriteAllText("/Quicktest.fsproj", projText)
    // let quicktest = Path.Join(__SOURCE_DIRECTORY__, "../tests/Fable.Tests.C/Fable.Tests.C.fsproj")
    // Compiler.compileSingleFile projFile // "C:/Users/Dave/projects/Fable/src/quicktest/QuickTest.fs"
    Compiler.compileProject testsProjFile
    0
