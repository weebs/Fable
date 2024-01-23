module Writer
open System; open FSharp.Reflection; open Ast; open FParsec
open FSharp.Data.LiteralProviders
open clang_ast_parser
open Fli
                                                             
let args = System.Environment.GetCommandLineArgs()
printfn $"Args = %A{args}"

let isFable = false
let runCmd (cmd: string) =
    cli {
        // Shell Shells.BASH
        Exec (cmd.Split(" ")[0])
        Arguments (cmd.Split(" ") |> Array.skip 1 |> String.concat " ")
        // Command cmd
    }
    |> Command.execute

let filePath =
    let path =
        if args.Length > 1 && args[1].EndsWith "clang-ast-parser.fsproj" = false then
            args[1] 
        else
           // "/tmp/bindings.txt"
           "/usr/include/portmidi.h"
    if path.EndsWith ".h" then
        let name = IO.Path.GetFileNameWithoutExtension path 
        let result = runCmd $"clang {path} -I. -std=c11 -Xclang -ast-dump -fsyntax-only -fno-color-diagnostics -Wno-visibility"
        let fileName = name + ".txt"
        IO.File.WriteAllText (name + ".txt", result.Text.Value)
        // let dir = IO.Path.GetDirectoryName filePath
        fileName
    else
        path

let inline debug (msg: 't) = System.Console.WriteLine $"%A{msg}"
try
    let fileText = printfn $"Reading file: {System.IO.Path.GetFullPath(filePath)}"; System.IO.File.ReadAllText(System.IO.Path.GetFullPath(filePath))

    let t =
        async {
            let! parsedContent = (File.parse fileText)
            let parsedContent = parsedContent |> Array.collect Seq.toArray
            let parsed = parsedContent |> Array.map (fun (line, e) -> match e with | Success (element, state, pos) -> Some (line, element) | _ -> None) |> Array.filter Option.isSome |> Array.map Option.get
            let failedToParse = parsedContent |> Array.map (fun (line, e) -> match e with | Failure (error, parserError, state) -> Some (line, error, parserError) | _ -> None) |> Array.filter Option.isSome |> Array.map Option.get
            let astNodeTypeNames = ()
            printfn $"Failed to Parse {failedToParse.Length} ?\n%A{failedToParse}"

            let elementNodeTypeNames = parsed |> Array.map (fun (line, e) -> e.Type) |> Array.distinct
            printfn $"Node types: \n"


            // for n in parsed do printfn $"%A{n}"
            for typ in elementNodeTypeNames do
                printfn $"{typ}"
            printfn ""
            let byType = 
                parsed |> Array.mapi (fun i item -> i, item) 
                |> Array.groupBy (fun (index, (line, parsed)) -> parsed.Type)
            for (typ, items) in byType do
                printfn $"{typ} "
            // printfn $"""%A{debug Parser.Type.parse "void (*)(struct SoundIO *, int)"}"""
            // |> ignore
            // let tuple = 
            //     manyCharsTill anyChar (pchar '(') .>>. pchar '*' .>>. manyCharsTill anyChar (pchar ')') .>>. manyCharsTill anyChar (pchar ')')
            // let tuple =
            //     many1Chars (noneOf [ '(' ]) .>>. pchar '('
            // debug Parser.Type.tuple "void (*foo)(int, void (*yo)(int, double, string, char*))"

            printfn "Declarations"
            let! fileDecls = parseFile parsed
            let file = 
                fileDecls 
                |> Array.collect id 
                |> Array.groupBy (fun item -> 
                    (FSharpValue.GetUnionFields (item, item.GetType()) |> fst).Name)
            let types = Map.ofArray file
            printfn $"%A{types.Keys |> Seq.toArray}"


            let typedefStructs = 
                types["Struct"] 
                |> Array.choose (fun (Struct (name, fields)) -> 
                    // let defs =
                        types["Typedef"] 
                        |> Array.tryPick (fun (Typedef (typedefAlias, typedefType)) -> 
                            // if typedefAlias = name
                            match typedefType with
                            | Parser.Type.ArgType.TypeName def when def = name ->
                                Some (name, fields, typedefAlias) 
                            | _ -> None)
                    // if defs then Some 
                )
            let typeDefs = types["Typedef"] |> Array.map (fun (Typedef (typedefAlias, typedefType)) -> Type.toFSharp typedefType, typedefAlias) |> Map.ofArray

            for (name, items, typedef) in typedefStructs do
                printfn $"""struct {name}"""
            printfn $"Items = {fileDecls |> Array.collect id |> Array.length}"
            // printfn $"Items = {|> Array.length}"
            let items =
                fileDecls
                |> Array.collect id
                |> Array.sortBy (function
                    | Function _ ->
                        5
                    | Typedef (s, Ast.Parser.Type.ArgType.FunctionPrototype _) ->
                        3
                    | Enum _ ->
                        1
                    | Typedef _ ->
                        0
                    | Struct _ ->
                        2
                    | _ ->
                        4)
            let moduleName = System.IO.Path.GetFileNameWithoutExtension filePath
            let sb = System.Text.StringBuilder()
            do
            // for itemType, items in file do
            // for item in fileDecls |> Array.filter (function | Typedef _ ->   true | _ -> false) do
                let inline printfn (string: string) = sb.AppendLine(string) |> ignore
                printfn $"module rec {moduleName}"
                if isFable then
                    printfn "open Fable.Core"
                    printfn "open C.Lib.Core"
                else
                    printfn "open System.Runtime.InteropServices"
                // printfn "let [<Literal>] moduleName = \"foo\""
                let ignoredStructs = [| "__fsid_t" |]
                for item in items do
                    match item with
                    | Var (name, typ) ->
                        printfn $"{name} : {Type.toFSharp typ}"
                    | Function (name, returnType, args) ->
                        // let text = 
                        //     args |> Array.map (fun (name, argType) -> 
                        //         $"{Type.toFSharp argType} {name.Trim()}") 
                        //     |> String.concat ", "
                        // let returnType =
                            // (Type.toFSharp returnType).Replace("unit", "void")
                        // printfn $"extern {returnType}({text})\n"
                        printfn (DefsGenerator.writeFunction moduleName isFable name returnType args)
                    | Struct (name, variables) ->
                        if not (Array.contains name ignoredStructs)  then
                            let def =  DefsGenerator.writeStruct isFable typeDefs name variables
                            printfn def
                        // let name =
                        //     if typeDefs.ContainsKey ("struct " + name) then
                        //         typeDefs["struct " + name]
                        //     else
                        //         name
                        //     // printf "typedef "
                        //
                        // // let printfn (string: string) = System.Console.WriteLine(string)
                        // if variables.Length = 0 then
                        //     printfn $"""type [<EmitType("{name}")>] [<Struct>] {name} = struct end"""
                        // else
                        //     printfn $"""type [<EmitType("{name}")>] [<Struct>] {name} = {{"""
                        //     for (name, typ) in variables do
                        //         let name = name.Replace("enum ", "").Replace("struct ", "")
                        //         // printfn $"""    {Type.toFSharp typ} {name};"""
                        //         printfn $"""    {name}: {Type.toFSharp (true, typ)}"""
                        //     printfn "}"

                        // printf "}"
                        // if typeDefs.ContainsKey ("struct " + name) then
                        //     let typedef = typeDefs["struct " + name]
                        //     printf $" {typedef}"
                        // printf ";\n"
                    | Enum (name, enumType, enumCases) ->
                        // debug name
                        // debug enumType
                        // debug (enumCases |> Array.map snd)
                        let enumCases =
                            if enumCases |> Array.forall (fun (Label (name, _type, value)) -> value = null) then
                                enumCases |> Array.mapi (fun i (Label (name, _type, _)) -> Label (name, _type, i))
                            else
                                enumCases
                        printfn $"type {name} ="
                        let suffix = 
                            match Array.head enumCases with
                            | Label (_, Parser.Type.ArgType.TypeName n, caseValue) ->
                                match n with
                                | "unsigned int" -> "u"
                                | "char" | "unsigned char" -> "uy"
                                | _ -> ""
                            | _ ->  ""
                        // let mutable index = 0
                        let mutable lastEnumValue = -1
                        for (Label (name, t, caseValue)) in enumCases do
                            // printfn $"    | {name} = {index}{suffix}"
                            let caseValue =
                                if caseValue = null then
                                    lastEnumValue + 1
                                else
                                    Convert.ToInt32 caseValue
                            lastEnumValue <- caseValue
                            printfn $"    | {name} = {caseValue}{suffix}"
                            // index <- index + 1
                        // printfn $"(* TODO ENUMS {name} // {enumType}\n%A{enumCases} *)"
                    | Typedef (name, typedefType) -> //when (Parser.Type.ArgType.TypeName name) <> typedefType ->
                        let ignoredDefs = [| "__builtin_va_list"; "__NSConstantString"; "__int128_t"; "__uint128_t"; "__uint128_t"; "khronos_boolean_enum_t" |]
                        // let printfn = ignore
                        let fsTypeDef = 
                            Type.toFSharp typedefType
                            |> function
                                | s when s.Contains "->" -> "\n    delegate of " + s
                                | s -> s
                        if not (Array.contains name ignoredDefs) then
                            if name <> fsTypeDef then
                                if fsTypeDef = "unit" then
                                    printfn $"type {name} = struct end"
                                else
                                    printfn $"type {name} = {fsTypeDef}"
                        // printfn $"typedef {name} {Type.toFSharp typedefType};"
                    // for item in (items |> Array.take 5) do
                    //     printfn $"%A{item}"
            printfn "Async!"
            // let file = sb.ToString().Replace("type khronos_boolean_enum_t = enum khronos_boolean_enum_t", "// TODO: ENUMS")
            // System.IO.File.WriteAllText(__SOURCE_DIRECTORY__ + "/opengl.fs", sb.ToString())
            let outPath = "lib." + System.IO.Path.GetFileNameWithoutExtension(filePath) + ".fs"
            // printfn $"{sb.ToString()}"
            let outputText =
                sb
                    .ToString()
                    .Replace("**", "* *")
                    .Replace("**", "* *")
                    .Replace("unsigned char", "byte")
            Console.WriteLine outputText
            printfn $"Writing FFI sources to ${outPath}"; System.IO.File.WriteAllText(outPath, outputText)
        }

    if args[0].EndsWith "clang-ast-parser.dll" then
        t |> Async.RunSynchronously
    else
        t |> Async.Start
    // debug Parser.functionDecl "addr <addr> addr add 'int (int, int)'"

    // debug Parser.functionDecl "0x55922a3ad7d8 <line:175:1, col:132> col:6 emscripten_dlopen 'void (const char *, int, void *, em_dlopen_callback, em_arg_callback_func)'"
    // debug tuple "void (*)(int, void (*)(int, double, string, char*))"
with error -> printfn $"{error}"
