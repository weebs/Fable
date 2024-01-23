module clang_ast_parser.DefsGenerator

open Ast

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
                String.concat "\n    " (fields |> Array.map (fun (name, typ) ->
                $"{name}: {Type.toFSharp (true, typ)}"))
            "{\n    " + fields + "\n}"
    $"""type{if isFable then " [<EmitType(\"" + name + "\")>]" else ""} [<Struct>] {name} = {fields}"""

let writeFunction moduleName isFable name returnType (args: (string * _)[]) : string =
    if isFable then
        let text = 
            if args.Length > 0 then
                args |> Array.map (fun (name, argType) -> 
                    $"({Type.fsharpName <| name.Trim()}: {Type.toFSharp argType})") 
                |> String.concat " "
            else "()"
        $"[<Emit(\"{name}\")>]\nlet {name} {text} : {Type.toFSharp returnType} = nativeOnly\n"
    else
        let args_text = 
            args |> Array.map (fun (name, argType) -> 
                $"{Type.toFSharpExtern argType} {Type.fsharpName <| name.Trim()}") 
            |> String.concat ", "
        $"[<DllImport(\"{moduleName}\")>]\nextern {Type.toFSharpExtern returnType} {name} ({args_text})"

module FFI =
    type ffiDelegate = delegate of int -> int
    // extern int foo (int id, ffiDelegate& callback)
    type rtcDataChannelInit = struct end
    extern int  rtcCreateDataChannelEx (unit pc, char * label, rtcDataChannelInit * init)
