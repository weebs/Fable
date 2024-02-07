module clang_ast_parser.DefsGenerator

open Ast
open Ast.Parser.Type
let writeStruct isFable (typeDefs: Map<string, string>) name (fields: (string * Parser.Type.ArgType)[]) =
    let name =
        if typeDefs.ContainsKey ("struct " + name) then
            typeDefs["struct " + name]
        else
            name
    let mutable safeInit = false
    let fieldsText =
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
                                safeInit <- true
                                $"nativeint (* {s}: {def} *)", []
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
    let safeInitDef =
        if not safeInit then ""
        else
            let safeInitFields =
                [|
                    for (name, t) in fields do
                        match t with
                        | TypeName s when typeDefs.ContainsKey s ->
                            $"{name}: {s}"
                        | _ ->
                            $"{name}: {fst (Type.toFSharp (name, true, t))}"
                |]
                |> String.concat "\n    "
            $"type {name}_SafeInit = {{
    {safeInitFields}
}}\n"
    let safeInit =
        if not safeInit then ""
        else
            let initFields =
                [|
                    for (name, t) in fields do
                        match t with
                        | TypeName s when typeDefs.ContainsKey s ->
                            let def = typeDefs[s]
                            if def.StartsWith "delegate " then
                                // $"{name} = System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate(init.{name})"
                                $"{name} = Marshal.GetFunctionPointerForDelegate(init.{name})"
                            else
                                $"{name} = init.{name}"
                        | _ -> $"{name} = init.{name}"
                |]
                |> String.concat "\n            "
            $" with
    static member Init (init: {name}_SafeInit) : {name} =
        {{
            {initFields}
        }}"
    $"""{safeInitDef}type{if isFable then " [<EmitType(\"" + name + "\")>]" else ""} [<Struct>] {name} = {fieldsText}{safeInit}"""

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
