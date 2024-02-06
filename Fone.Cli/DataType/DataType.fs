module Fone.DataType.Core

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.C
open Fable.C.AST
open System.Text

let writeStruct (generics: (string * Type) list) (ent: Entity) (ctor: MemberDecl option) (struct_name: string, compiledFields: (string * C.Type) list) =
    if true || struct_name.Contains "Array" then
        printfn $"struct {struct_name} %A{compiledFields}"
    if ent.BaseType |> Option.map (fun t -> t.Entity.FullName) = Some "System.Attribute" then
        printfn $"Ignoring type that inherits from [<Attribute>] {struct_name} \n%A{ent}"
        ""
    else
        let _sb = StringBuilder()
        _sb
            .AppendLine($"typedef struct {struct_name} {{")
        |> ignore
        if not (ent.IsValueType) && not (ent.IsFSharpUnion) then
            _sb.AppendLine("    unsigned char __refcount;")
            |> ignore
        for fieldName, fieldType in compiledFields do
            match fieldType with
            | C.Ptr (C.UserDefined (fullName, isValueType, entityRef))
            | C.UserDefined(fullName, isValueType, entityRef) ->
                let prefix =
                    if entityRef.IsSome && entityRef.Value.IsFSharpUnion then " /* union */" else "" //"struct"
                _sb.AppendLine($"   {prefix} {fieldType.ToTypeString()} {fieldName};")
                |> ignore
            | _ ->
                _sb.AppendLine($"    {fieldType.ToTypeString()} {fieldName};")
                |> ignore
        _sb.AppendLine $"}} {struct_name};"
        |> ignore
        if not ent.IsFSharpUnion then
            match ent.IsValueType with
            | true ->
                // _sb.AppendLine $"struct {struct_name} {struct_name}_ctor();" |> ignore
                match ctor with
                | Some c ->
                    let paramsText =
                        c.Args
                        |> List.map (fun a -> a.Name, Fable.C.Transforms.transformType generics a.Type)
                        |> List.map (fun (name, t) -> $"{t.ToTypeString()} {name}")
                        |> String.concat ", "
                    _sb.AppendLine $"{struct_name} {struct_name}_ctor({paramsText});" |> ignore
                | None ->
                    ()
            | false ->
                print.printfn $"{ent.FullName}: {if ent.BaseType.IsSome then ent.BaseType.Value.Entity.FullName else string '\n'}"
                // _sb.AppendLine $"struct {struct_name}* {struct_name}_ctor();" |> ignore
                // todo: add this from somewhere else?
                match ctor with
                | Some c ->
                    let paramsText =
                        c.Args
                        |> List.map (fun a -> a.Name, Fable.C.Transforms.transformType generics a.Type)
                        |> List.map (fun (name, t) -> $"{t.ToTypeString()} {name}")
                        |> String.concat ", "
                    _sb.AppendLine $"{struct_name}* {struct_name}_ctor({paramsText});" |> ignore
                | None ->
                    if ent.IsFSharpRecord then
                        if not ent.IsValueType then
                            _sb.AppendLine $"{struct_name}* {struct_name}_ctor({struct_name} data);" |> ignore
                    else
                        // _sb.AppendLine $"{struct_name}* {struct_name}_ctor();" |> ignore
                        ()
                _sb.AppendLine $"void {struct_name}_Destructor({struct_name}* this$);" |> ignore
        if Helpers.Query.hasFinalizer ent then
            _sb.AppendLine $"void* {struct_name}_Finalize({struct_name}* this$);"
            |> ignore
        _sb.ToString()

let writeUnionToBuilder (genArgs: Type list) (sb: CompiledOutputBuilder) (ent: Entity) =
    let generics =
        genArgs
        |> List.zip (ent.GenericParameters |> List.map _.Name)
    let getCaseInfo (case: UnionCase) : string * (string * C.Type) list =
        let fields = case.UnionCaseFields
        Helpers.Print.unionCaseName(Transforms.transformType generics, genArgs, case), [
            for f in fields do
//                            let f2 = f :?> FsField
                // yield (f.DisplayName, transformType [] f.FieldType)
                // todo: merp
                yield (f.Name, Transforms.transformType generics f.FieldType)
        ]
    let cases =
        ent.UnionCases
        |> List.map getCaseInfo
        |> List.map (writeStruct generics ent None)
    let fieldNames = ent.UnionCases |> List.map (fun c -> $"""{Helpers.Print.unionCaseName (Transforms.transformType generics, genArgs, c)} {c.Name};""")
    let fullName = Helpers.Print.unionName (Transforms.transformType generics, genArgs, ent)
    let union = $"""
typedef union {fullName}_UnionData {{
    {System.String.Join("\n    ", fieldNames)}
}} {fullName}_UnionData;
typedef struct {fullName} {{
    unsigned char __refcount;
    int union_tag;
    union {fullName}_UnionData union_data;
}} {fullName};
"""
// typedef union {fullName}_UnionData {{
//     {System.String.Join("\n    ", fieldNames)}
// }} {fullName}_UnionData;
// typedef struct {fullName} {{
//     int union_tag;
//     union {fullName}_UnionData union_data;
// }} {fullName};
//                    cases |> List.iter (fun (name, fields) -> writeStruct ent name fields)
    cases |> List.iter (sb.AppendLine >> ignore)
    sb.AppendLine union |> ignore
    if not ent.IsValueType then
        let cleanup = [|
            for tag in 0..ent.UnionCases.Length - 1 do
                let case = ent.UnionCases[tag]
                let toCall = [|
                    for field in case.UnionCaseFields do
                        if Transforms.requiresTracking generics field.FieldType then
                            let t = Transforms.transformType generics field.FieldType
                            let name =
                                match t with
                                | C.Ptr t -> t.ToNameString()
                                | t -> t.ToNameString()
                            $"    Runtime_end_var_scope(this$->union_data.{case.Name}.{field.Name}, {name}_Destructor);"
                |]
                if toCall.Length > 0 then
                    $"if ({tag} == this$->union_tag) {{\n" +
                    System.String.Join("\n    else ", toCall) + "\n    }"
        |]
        let destructorSig = $"void {fullName}_Destructor({fullName}* this$)"
        let declarations =
            $"{destructorSig};"
        let code = $"""
{destructorSig} {{
    {cleanup |> String.concat "\n"}
    free(this$);
}}"""
        let caseConstructors = [
            for (tag, case) in ent.UnionCases |> List.mapi (fun index case -> index, case) do
                let info = getCaseInfo case
                let argText (arg: Field) =
                    let t = Transforms.transformType generics arg.FieldType
                    $"{t.ToTypeString()} {arg.Name}"
                let args = case.UnionCaseFields |> List.map argText |> String.concat ", "
                let assignments =
                    case.UnionCaseFields
                    |> List.map (fun field ->
                        $"this$->union_data.{case.Name}.{field.Name} = {field.Name};" +
                        (if Transforms.requiresTracking generics field.FieldType then
                            $"\n    this$->union_data.{case.Name}.{field.Name}->__refcount++;"
                         else
                            "")
                    )
                let name = $"{fullName}_{case.Name}_ctor"
                $"""
{fullName}* {name}({args}) {{
    {fullName}* this$ = ({fullName}*)malloc(sizeof({fullName}));
    this$->__refcount = 1;
    this$->union_tag = {tag};
    {assignments |> String.concat "\n    "}
    return Runtime_autorelease(this$, {fullName}_Destructor);
}}
"""
        ]
        // sb.AppendLine (caseConstructors |> String.concat "\n") |> ignore
        let code = code + "\n" + (caseConstructors |> String.concat "\n")
        {| decl = declarations; code = code |}
    else
        {| decl = ""; code = "" |}
let writeArray arrayType =
    let t = (Transforms.transformType [] arrayType)
    // todo: Arrays of arrays
    // todo: I think get_Item needs to use Autorelease/increment count
        // ex: foo[index] is used as param to function f, f calls g, value gets stored into a variable in g, and that variable goes out of scope
        // ex: foo[index] is used a param to function f and so is foo, then the value is removed from foo in f
    let typeName = $"System_Array__{t.ToNameString()}"
    let decl = $"
typedef struct System_Array__{t.ToNameString()} {{
    unsigned char __refcount;
    int length;
    {t.ToTypeString()}* data;
}} System_Array__{t.ToNameString()};

{typeName}* {typeName}_ctor(int size, {t.ToTypeString()}* data);
void {typeName}_Destructor({typeName}* this$);

void {typeName}_set_Item({typeName}* this$, int index, {t.ToTypeString()} value);
{t.ToTypeString ()} {typeName}_get_Item({typeName}* this$, int index);
{typeName}* {typeName}_alloc(int size);
"
    let ctor = $"""
{typeName}* {typeName}_ctor(int size, {t.ToTypeString()}* data) {{
    {typeName}* this$ = malloc(sizeof({typeName}));
    this$->__refcount = 1;
    this$->length = size;
    this$->data = malloc(sizeof({t.ToTypeString()}) * size);
    for (int i = 0; i < size; i++) {{
        this$->data[i] = data[i];
        {if Transforms.requiresTracking [] arrayType then "this$->data[i]->__refcount++;" else ""}
    }}
    return ({typeName}*)Runtime_autorelease(this$, {typeName}_Destructor);
}}

{typeName}* {typeName}_alloc(int size) {{
    {typeName}* this$ = malloc(sizeof({typeName}));
    this$->__refcount = 1;
    this$->length = size;
    this$->data = calloc(size, sizeof({t.ToTypeString()}));
    // TODO: initialize values
    return ({typeName}*)Runtime_autorelease(this$, {typeName}_Destructor);
}}

void {typeName}_set_Item({typeName}* this$, int index, {t.ToTypeString()} value) {{
    if (index > this$->length) {{
        exit(1);
    }}
    if (index < 0) {{
        exit(index);
    }}
    {
        if Transforms.requiresTracking [] arrayType then
            let name = t.ToNameString()
            let tName = name.Substring(0, name.Length - 3)
            let destructor = tName + "_Destructor"
            $"Runtime_swap_value((void*)(this$->data + index), value, " + destructor + ");"
        else
            "this$->data[index] = value;"
    }
}}

{t.ToTypeString ()} {typeName}_get_Item({typeName}* this$, int index) {{
    if (index > this$->length) {{
        exit(1);
    }}
    if (index < 0) {{
        exit(index);
    }}
    return this$->data[index];
}}
void {typeName}_Destructor({typeName}* this$) {{
    {
        if (Transforms.requiresTracking [] arrayType) then
            match t with
            | C.AST.C.Ptr t ->
                $"
    for (int i = 0; i < this$->length; i++) {{
        if (this$->data[i] == NULL) {{ continue; }}
        Runtime_end_var_scope(this$->data[i], {t.ToNameString ()}_Destructor);
    }}
                "
            | _else ->
                ""
        else
            ""
    }
    free(this$->data);
    free(this$);
}}
"""
    {| name = typeName; decl = decl; code = ctor |}
