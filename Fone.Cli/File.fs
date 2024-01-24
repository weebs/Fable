module Fable.C.File
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
let writeFile (sourceFilePath: string) (includes: string list) (compiledModule: Map<string, C.ModuleDeclaration> ref) (static_constructor: (ActionDecl * C.Statement list) list) =
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
let transformFile (_com: Fable.Compiler) (file: File) =
    // database.contents <- _com
    print.printfn $"Database: %A{database.contents}"
    compiler.UpdateFile(_com.CurrentFile, FileCompilationResults.Empty, ())
    let context = { currentFile = _com.CurrentFile; idents = [] }
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
            compiledModule += (memberDecl.Name, C.StaticVar { _type = transformType [] memberDecl.Body.Type; name = memberDecl.Name; value = value; requiresTracking = false })
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
                    let emitTypeAttr = ent.Attributes |> Seq.tryFind _.Entity.FullName.Contains("EmitType") // = Const.emitType)
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
                            let emitTypeAttr = ent.Attributes |> Seq.tryFind _.Entity.FullName.Contains("EmitType") // = Const.emitType)
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
let writeModule (fileName: string) (_module: CompiledAst) : string =
    writeFile fileName _module.includes _module.compiledModule _module.static_constructor
let writeNesModule (fileName: string) (_module: CompiledAst) : string =
    writeModule fileName _module
    |> fun text -> text.Replace("
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>", "")
