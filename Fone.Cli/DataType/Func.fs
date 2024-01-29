module Fone.DataType.Func

open Fable.C.Helpers
open Fable.C.Transforms
let write argsTypes fnReturn =
    // let argsTypes = argsTypes |> List.filter (function | Fable.AST.Fable.Unit -> false | _ -> true)
    let typeName = Print.funcTypeName (transformType [], argsTypes, fnReturn)
    let returnType = transformType [] fnReturn
    let returnTypeName = returnType |> _.ToTypeString()
    let argsTypes = argsTypes |> List.map (transformType [])
    let argsTypesStrings = argsTypes |> List.map _.ToTypeString()
    let paramsText =
        [| for i in 0..(argsTypesStrings.Length - 1) do
               $"{argsTypesStrings[i]} arg_{i}" |]
        |> String.concat ", "
    let callArgs =
        [| for i in 0..argsTypesStrings.Length - 1 do $"arg_{i}" |]
        |> String.concat ", "
    let ptrType0 =
        $"""{returnTypeName} (*)({argsTypesStrings |> String.concat ", "})"""
    let ptrType1 =
        $"""{returnTypeName} (*)({("void*" :: argsTypesStrings) |> String.concat ", "})"""
    let destructorSig = $"void {typeName}_Destructor({typeName}* this$)"
    let decl = $"""
typedef struct {typeName} {{
    unsigned char __refcount;
    int tag;
    void (*fp)(void*);
    void* data;
    void* data_destructor;
}} {typeName};

{destructorSig};
"""
    let impl = $"""
{destructorSig} {{
    if (this$->tag == 1) {{
        ((void (*)(void*))this$->data_destructor)(this$->data);
    }}
    free(this$);
}}
{typeName}* {typeName}_ctor(int tag, void* fp, void* data, void* data_destructor) {{
    {typeName}* this$ = malloc(sizeof({typeName}));
    this$->__refcount = 1;
    this$->fp = fp;
    this$->tag = tag;
    if (tag == 1) {{
        this$->fp = fp;
        this$->data = data;
        this$->data_destructor = data_destructor;
    }}
    return Runtime_autorelease(this$, {typeName}_Destructor);
}}
// todo: autorelease? (No, not needed if not modifying __thread_context (todo: Test)
{returnTypeName} {typeName}_Invoke({typeName}* this$, {paramsText}) {{
    if (this$->tag == 0) {{
        return (({ptrType0})this$->fp)({callArgs});
    }} else {{
        return (({ptrType1})this$->fp)(this$->data, {callArgs});
    }}
}}
"""
    {| decl = decl; code = impl |}

let writeClosure captured types __return =
    // added_types
    // let captured = captured |> List.filter (function | Fable.AST.Fable.Unit -> false | _ -> true)
    // let types = types |> List.filter (function | Fable.AST.Fable.Unit -> false | _ -> true)
    let capturedArgsTypes = captured |> List.map (transformType []) |> List.map _.ToTypeString()
    let invokeArgsTypes = types |> List.map (transformType []) |> List.map _.ToTypeString()
    let invokeParamsText =
        [| for i in 0..(invokeArgsTypes.Length - 1) do $"{invokeArgsTypes[i]} arg_{i}" |]
        |> String.concat ", "
    let callArgs =
        [| for i in 0..invokeArgsTypes.Length - 1 do $"arg_{i}" |]
        |> String.concat ", "
    let capturedArgs =
        [| for i in 0..capturedArgsTypes.Length - 1 do $"var_{i}" |]
        |> String.concat ", "
    let capturedParamsText =
        [| for i in 0..capturedArgsTypes.Length - 1 do $"{captured[i] |> transformType [] |> _.ToTypeString()} var_{i}" |]
        |> String.concat ", "
    let variables =
        [|
        for i in 0..capturedArgsTypes.Length - 1 do
            if requiresTracking [] captured[i] then
                $"{capturedArgsTypes[i]} var_{i} = ({capturedArgsTypes[i]})this$->capturedArgs[{i}];"
            else
                $"{capturedArgsTypes[i]} var_{i} = *({capturedArgsTypes[i]}*)this$->capturedArgs[{i}];"
        |]
        |> String.concat "\n    "
    let ctorCapturing =
        [|
        for i in 0..captured.Length - 1 do
            let t = transformType [] captured[i]
            if requiresTracking [] captured[i] then
                yield $"{t.ToTypeString()} ptr_{i} = var_{i};"
                yield $"var_{i}->__refcount++;"
            else
                yield $"{t.ToTypeString()}* ptr_{i} = malloc(sizeof({t.ToTypeString()}));"
                yield $"*ptr_{i} = var_{i};"
            yield $"this$->capturedArgs[{i}] = (void*)ptr_{i};"
        |]
        |> String.concat "\n    "
    let destructorStatements =
        [|
            for i in 0..captured.Length - 1 do
                if requiresTracking [] captured[i] then
                    let t = transformType [] captured[i]
                    match t with
                    | Fable.C.AST.C.Ptr t ->
                        $"Runtime_end_var_scope(this$->capturedArgs[{i}], {t.ToNameString()}_Destructor);"
                    | _ ->
                        $"Runtime_end_var_scope(this$->capturedArgs[{i}], {t.ToNameString()}_Destructor);"
                else
                    $"free(this$->capturedArgs[{i}]);"
        |]
        |> String.concat "\n    "
    let returnType = transformType [] __return |> _.ToTypeString()
    let ptrType =
        let argsTypesStrings =
            capturedArgsTypes @ invokeArgsTypes
        $"""{returnType} (*)({argsTypesStrings |> String.concat ", "})"""
    let typeName = Print.closureTypeName (transformType [], captured, types, __return)
    let decl = $"""
typedef struct {typeName} {{
    // unsigned char __refcount;
    void** capturedArgs;
    void (*fp)(void*);
}} {typeName};
                """
                // todo: type safety for FP
    let impl = $"""
{typeName}* {typeName}_ctor({capturedParamsText}, void (*fp)(void*)) {{
    {typeName}* this$ = ({typeName}*)malloc(sizeof({typeName}));
    this$->capturedArgs = malloc(sizeof(void*) * {captured.Length});
    {ctorCapturing}
    this$->fp = fp;
}}
void {typeName}_Destructor(void* data) {{
    {typeName}* this$ = ({typeName}*)data;
    {destructorStatements}
    free(this$->capturedArgs);
    free(this$);
}}
{returnType} {typeName}_Invoke(void* closure, {invokeParamsText}) {{
    {typeName}* this$ = ({typeName}*)closure;
    {variables}
    return (({ptrType})this$->fp)({capturedArgs}, {callArgs});
}}
                """
    {| decl = decl; code = impl |}
