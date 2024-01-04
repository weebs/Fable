module Fone.DataType.Core

open Fable
open Fable.AST
open Fable.AST.Fable
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
            .AppendLine("    unsigned char __refcount;")
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
