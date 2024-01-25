module Fone.DataType.Core

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.C
open Fable.C.AST
open System.Text

let writeStruct (ent: Entity) (struct_name: string, compiledFields: (string * C.Type) list) =
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
                _sb.AppendLine $"{struct_name} {struct_name}_ctor();" |> ignore
            | false ->
                print.printfn $"{ent.FullName}: {if ent.BaseType.IsSome then ent.BaseType.Value.Entity.FullName else string '\n'}"
                // _sb.AppendLine $"struct {struct_name}* {struct_name}_ctor();" |> ignore
                _sb.AppendLine $"{struct_name}* {struct_name}_ctor();" |> ignore
                _sb.AppendLine $"void {struct_name}_Destructor({struct_name}* this$);" |> ignore
        _sb.ToString()

let writeUnionToBuilder (sb: CompiledOutputBuilder) (ent: Entity) =
                    let getCaseInfo (case: UnionCase) : string * (string * C.Type) list =
                        let fields = case.UnionCaseFields
                        case.FullName.Replace(".", "_"), [
                            for f in fields do
    //                            let f2 = f :?> FsField
                                // yield (f.DisplayName, transformType [] f.FieldType)
                                // todo: merp
                                yield (f.Name, Transforms.transformType [] f.FieldType)
                        ]
                    let cases =
                        ent.UnionCases
                        |> List.map getCaseInfo
                        |> List.map (writeStruct ent)
                    let fieldNames = ent.UnionCases |> List.map (fun c -> $"""{c.FullName.Replace(".", "_")} {c.Name};""")
                    let fullName = ent.FullName.Replace(".", "_")
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
                                        if Transforms.requiresTracking [] field.FieldType then
                                            let t = Transforms.transformType [] field.FieldType
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
                        let finalizer = $"""
void {fullName}_Destructor({fullName}* this$) {{
    {cleanup |> String.concat "\n"}
    free(this$);
}}"""
                        sb.AppendLine finalizer
                        |> ignore
                        let caseConstructors = [
                            for (tag, case) in ent.UnionCases |> List.mapi (fun index case -> index, case) do
                                let info = getCaseInfo case
                                let argText (arg: Field) =
                                    let t = Transforms.transformType [] arg.FieldType
                                    $"{t.ToTypeString()} {arg.Name}"
                                let args = case.UnionCaseFields |> List.map argText |> String.concat ", "
                                let assignments =
                                    case.UnionCaseFields
                                    |> List.map (fun field ->
                                        $"this$->union_data.{case.Name}.{field.Name} = {field.Name};" +
                                        (if Transforms.requiresTracking [] field.FieldType then
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
                        sb.AppendLine (caseConstructors |> String.concat "\n") |> ignore
