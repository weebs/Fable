module clang_ast_parser.DefsGenerator

open Ast
open Ast.Parser.Type
let writeStruct isFable (typeDefs: Map<string, string>) name (fields: (string * Parser.Type.ArgType)[]) =
    let name =
        if typeDefs.ContainsKey ("struct " + name) then
            typeDefs["struct " + name]
        else
            name
    let fields =
        if fields.Length = 0 then "struct end"
        else
            let fields =
                fields
                |> Array.map (fun (name, typ) ->
                    // todo: typDeps
                    let typString, typDeps =
                        match typ with
                        | TypeName s when typeDefs.ContainsKey s ->
                            let def = typeDefs[s]
                            if def.StartsWith "delegate " then
                                $"nativeint (* {def} *)", []
                            else
                                Type.toFSharp (name, true, typ)
                        | _ ->
                            Type.toFSharp (name, true, typ)
                    $"{Type.fsharpName name}: {typString}"
                )
                |> String.concat "\n    "
                // String.concat "\n    " (fields |> Array.map (fun (name, typ) ->
                // $"{Type.fsharpName name}: {Type.toFSharp (true, typ)}"))
            "{\n    " + fields + "\n}"
    $"""type{if isFable then " [<EmitType(\"" + name + "\")>]" else ""} [<Struct>] {name} = {fields}"""

let writeFunction moduleName isFable name returnType (args: (string * _)[]) : string =
    if isFable then
        let text =
            if args.Length > 0 then
                args
                |> Array.map (fun (name, argType) ->
                    // todo: argTypeDeps
                    let argTypeString, argTypeDeps = Type.toFSharp (name, argType)
                    $"({Type.fsharpName <| name.Trim()}: {argTypeString})")
                |> String.concat " "
            else "()"
        // todo:
        let returnTypeString, returnTypeDeps = Type.toFSharp (name, returnType)
        $"[<Emit(\"{name}\")>]\nlet {name} {text} : {returnTypeString} = nativeOnly\n"
    else
        let args_text =
            args
            |> Array.map (fun (name, argType) ->
                $"{Type.toFSharpExtern argType} {Type.fsharpName <| name.Trim()}")
            |> String.concat ", "
        $"[<DllImport(\"{moduleName}\")>]\nextern {Type.toFSharpExtern returnType} {name} ({args_text})"

module FFI =
    type ffiDelegate = delegate of int -> int
    // extern int foo (int id, ffiDelegate& callback)
    type rtcDataChannelInit = struct end
    extern int  rtcCreateDataChannelEx (unit pc, char * label, rtcDataChannelInit * init)
