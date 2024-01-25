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
                Core.writeUnionToBuilder sb ent
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
