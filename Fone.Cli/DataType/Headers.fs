module Fone.DataType.Headers
open System
open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.C.AST
open Fable.C
open Fable.C.Helpers
open Fable.C.Transforms
open System.Collections

/// Write the declarations of an individual file in the project
let writeFilesModuleHeaderStuffs (methods_sb: CompiledOutputBuilder) (sb: CompiledOutputBuilder) (file: string) =
    let fileName = IO.Path.GetFileName(file)
    sb.AppendLine ($"\n/* Contents for {fileName} */") |> ignore
    methods_sb.AppendLine ($"\n/* Contents for {fileName} */") |> ignore
    for kv in compiler.GetResult(file).memberDeclarations do
        let (_m, functionInfo) = kv.Value
        try
//            let method_name = Print.compiledMethodName(m.Value.Name, [], database.contents.GetMember(m.Value.MemberRef).DeclaringEntity.Value)
//            let type_sig = Print.compiledTypeSignature ([], transformType, database.contents, m.Value)
            let type_sig = Print.compiledFunctionSignature functionInfo
//            let arg_string = Compiler.writeFunctionArgs (m.Value.Args |> List.map (fun a -> (a.Name, transformType [] a.Type)))
            methods_sb.AppendLine $"{type_sig}" |> ignore
        with ex ->
            ()
    #if FABLE_COMPILER
    let declaredTypes = Generic.HashSet<string>()
    #else
    let declaredTypes = Generic.SortedSet<string>()
    #endif
    let ordered_classes = Generic.List<_>()
    let classDeclarations = Generic.Dictionary<_,_>()
    print.printfn $"=============== {file} Module header stuffs =====================" |> ignore
    for kv in compiler.GetResult(file).classDeclarations do
        classDeclarations.Add (kv.Key, (kv.Value, Query.classDeclDependencies compiler kv.Value))
    print.printfn $"ClassDeclarations Count: {classDeclarations.Count}" |> ignore
    let _members = compiler.GetResult(file).memberDeclarations
    print.printfn $"Method declaration count {_members.Count}" |> ignore
    let mutable lastCount = classDeclarations.Count
    print.printfn "Class delcarations: " |> ignore
    while classDeclarations.Count > 0 do
        let keys = classDeclarations.Keys
        for k in keys do
            let (c, deps) = classDeclarations.[k]
            if (deps |> List.forall declaredTypes.Contains) then
                declaredTypes.Add(k) |> ignore
                ordered_classes.Add(c)
                classDeclarations.Remove(k) |> ignore
        if classDeclarations.Count = lastCount then
            let keys = classDeclarations.Keys
            for k in keys do
                ordered_classes.Add(fst classDeclarations.[k])
                classDeclarations.Remove(k)
            // failwith "Unable to resolve dependency order"
        lastCount <- classDeclarations.Count
    for c in ordered_classes do
        print.printfn $"Writing type %A{c.Entity.FullName}" |> ignore
        match database.contents.TryGetEntity(c.Entity) with
        | Some ent ->
            if ent.IsFSharpUnion then
                let getCaseInfo (case: UnionCase) : string * (string * C.Type) list =
                    let fields = case.UnionCaseFields
                    case.FullName.Replace(".", "_"), [
                        for f in fields do
//                            let f2 = f :?> FsField
                            // yield (f.DisplayName, transformType [] f.FieldType)
                            // todo: merp
                            yield (f.Name, transformType [] f.FieldType)
                    ]
                let writeUnion (ent: Entity) =
                    let cases =
                        ent.UnionCases
                        |> List.map getCaseInfo
                        |> List.map (Core.writeStruct ent)
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
                                        if requiresTracking [] field.FieldType then
                                            let t = transformType [] field.FieldType
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
                                    let t = transformType [] arg.FieldType
                                    $"{t.ToTypeString()} {arg.Name}"
                                let args = case.UnionCaseFields |> List.map argText |> String.concat ", "
                                let assignments =
                                    case.UnionCaseFields
                                    |> List.map (fun field ->
                                        $"this$->union_data.{case.Name}.{field.Name} = {field.Name};" +
                                        (if requiresTracking [] field.FieldType then
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
                writeUnion ent
                //sb.AppendLine $"%A{ent}" |> ignore
            else
                let struct_name =
                    Print.compiledTypeName c.Entity
                let fields = compiler.GetEntity(c.Entity.FullName).FSharpFields
                let compiledFields =
                    fields |> List.map (fun f -> f.Name, (transformType [] f.FieldType))
                sb.Append(Core.writeStruct ent (struct_name, compiledFields))
                |> ignore
        | None ->
            ()
    sb.Append ($"/* End of contents for {fileName} */\n") |> ignore
    methods_sb.Append ($"/* End of contents for {fileName} */\n") |> ignore
