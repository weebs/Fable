module Fable.C.Compiler

open System.Text
open AST
open Fable.AST.Fable

module Interfaces =
    open Fable.AST

    type ICompiler =
        abstract member transformFunc: string -> Fable.Ident list -> Fable.Expr -> (string * Fable.Type) list -> C.FunctionInfo

let com = ref Unchecked.defaultof<Fable.Compiler>

// let mutable icompiler = Unchecked.defaultof<Interfaces.ICompiler>

let mutable writeStatement: C.Statement -> SourceBuilder = Unchecked.defaultof<_>
let mutable writeExpression: C.Expr -> string = Unchecked.defaultof<_>

let __struct = ""

// type Context =
//     { compiler: Fable.Compiler
//       includes: string list
//       compiled_module: Map<string, C.ModuleDeclaration>
//       class_generics: Map<string, Fable.AST.Fable.ClassDecl>
//       generic_defs: Map<string, Fable.AST.Fable.MemberDecl>
//       generic_impls: Map<string, (C.Type list) list>
//       generic_structs: Map<string, (C.Type list) list>
//       generics: Map<string, C.Type> }
//
// module Context =
//     // todo: add the C module stuff here
//     let addGenericImpl (name: string) (args: C.Type list) (context: Context) =
//         if context.generic_impls.ContainsKey(name) then
//             let existing = context.generic_impls[name]
//             if existing |> List.contains args then
//                 context
//             else
//                 { context with generic_impls = Map.add name (args :: existing) context.generic_impls }
//         else
//             { context with generic_impls = Map.add name [ args ] context.generic_impls }
//     let addGenericType (name: string) (args: C.Type list) (context: Context) =
//         if context.generic_structs.ContainsKey(name) then
//             let existing = context.generic_structs[name]
//             if existing |> List.contains args then
//                 context
//             else
//                 { context with generic_structs = Map.add name (args :: existing) context.generic_structs }
//         else
//             { context with generic_structs = Map.add name [ args ] context.generic_structs }

type FSharp.Collections.List<'t> with
    member this.trySkip (i: int) =
        if this.Length >= i then
            List.skip i this
        else
            []
//module Json =
//    open System.Text.Json
//    open System.Text.Json.Serialization
//
//    let options = JsonSerializerOptions()
//    options.Converters.Add(JsonFSharpConverter())
//
//    let serialize o = JsonSerializer.Serialize(o, options)
//    let deserialize<'t> (s: string) = JsonSerializer.Deserialize<'t>(s, options)

let writeArg ((name, _type): string * C.Type) =
    match _type with
    // | C.UserDefined(fullName, isValueType, entityRef) ->
    //     $"{_type.ToTypeString()} {name}"
    //     // $"{__struct}{fullName} {name}"
    // | C.Ptr (C.UserDefined (fullName, isValueType, entityRef)) ->
    //     // $"{__struct}{fullName}* {name}"
    //     $"{_type.ToTypeString()}* {name}"
    // | C.Ptr t ->
    //     $"{t.ToTypeString()}* {name}"
    | C.FunctionPtr(types, returnType) ->
        let s = System.String.Join(", ", types |> List.map (fun t -> t.ToTypeString()))
        $"{returnType.ToTypeString()} (*{name})({s})"
    | _ ->
        $"{_type.ToTypeString()} {name}"
let writeFunctionArgs (args: (string * C.Type) list) =
    if args.Length = 1 && (snd args[0]) = C.Void then
        ""
    else
        System.String.Join(", ", List.map writeArg args)

let valueToString (value: C.ValueKind) : string =
    match value with
    | C.ValueKind.CStr s -> $"\"{s}\""
    | C.ValueKind.Char c ->
        let s = (string c).Replace("\n", "\\n").Replace("\t", "\\t")
        $"'{s}'"
    | C.ValueKind.Void -> ""
    | C.ValueKind.Bool b ->
        b.ToString().ToLower()
    | C.ValueKind.UnionConstructorCall expr ->
        writeExpression expr
    | C.ValueKind.Compound (fieldValues, entity, fieldInfos, generics) ->
        let fields =
            if entity.IsValueType then List.zip fieldValues fieldInfos
            else List.zip fieldValues (("__refcount", C.Int) :: fieldInfos)
        // todo: replaced for NES cc65
        // let valueStrings = fields |> List.map (fun (expr, (name, _type)) -> $".{name} = {writeExpression expr}")
        let valueStrings = fields |> List.map (fun (expr, (name, _type)) -> $"{writeExpression expr}")
        let values = System.String.Join(", ", valueStrings)
        let structName =
            match entity.Attributes |> Seq.tryFind (fun attr -> attr.Entity.FullName.Contains("EmitType")) with
            | Some attr -> attr.ConstructorArgs.[0]
            | _ ->
                entity.FullName.Replace(".", "_")
                    .Replace($"`{generics.Length}", "__" + System.String.Join("_", generics |> List.map (fun g -> g.ToNameString())))
                    .Replace($"${generics.Length}", "__" + System.String.Join("_", generics |> List.map (fun g -> g.ToNameString())))
        // todo: replaced for NES cc65
        $"(struct {structName}){{{values}}}".Replace(";", ";\n    ")
        // $"{{{values}}}".Replace(";", ";\n    ")
    | C.ValueKind.ObjectCompound (structName, values) ->
        let sb = SourceBuilder()
         //{{{values}}}".Replace(";", ";\n    ")
        // sb.AppendLine($"(struct {structName}) {{")
        sb.AppendLine($"(struct {structName}) {{") |> ignore
        for value in values do
            sb.AppendLine($"    {writeExpression value},")
            |> ignore
        sb.AppendLine($"}}") |> ignore
        sb.ToString()
    | C.ValueKind.AnonymousCompound (values, entity, generics) ->
        let structName =
            match entity.Attributes |> Seq.tryFind (fun attr -> attr.Entity.FullName.Contains("EmitType")) with
            | Some attr -> attr.ConstructorArgs.[0]
            | _ ->
                entity.FullName.Replace(".", "_")
                    .Replace($"`{generics.Length}", "__" + System.String.Join("_", generics |> List.map (fun g -> g.ToNameString())))
                    .Replace($"${generics.Length}", "__" + System.String.Join("_", generics |> List.map (fun g -> g.ToNameString())))
        let sb = SourceBuilder()
         //{{{values}}}".Replace(";", ";\n    ")
        // sb.AppendLine($"(struct {structName}) {{")
        sb.AppendLine($"(struct {structName}) {{") |> ignore
        for value in values do
            sb.AppendLine($"    {writeExpression value},")
            |> ignore
        sb.AppendLine($"}}") |> ignore
        sb.ToString()
    | C.ValueKind.Int i -> string i
    | _ ->
        // Most of the time we can call toString on the second param (ex: 248.ToString() -> "248")
        (Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, typeof<C.ValueKind>) |> snd)[0] |>
            function
            | :? double as d ->
                let n = string d
                if n.Contains "." = false then n + ".0"
                else n
            | s -> string s
let rec _writeExpr (expr: C.Expr) : string =
    match expr with
    | C.BlockExpr statements ->
        let sb = SourceBuilder()
        sb
            .AppendLine("(")
            .BeginBlock(fun sb ->
                sb.AppendBuilder(writeStatement (C.Block statements))
//                for s in statements do
//                    sb.AppendStatement(s)
//                    |> ignore
            )
            .AppendLine(")")
            .ToString()
    | C.Ternary(cond, onTrue, onFalse) ->
        $"{_writeExpr cond} ? {_writeExpr onTrue} : {_writeExpr onFalse}"
    | C.Binary (op, left, right) ->
        let op_string =
            match op with
            | C.Add -> "+"
            | C.Mult -> "*"
            | C.Minus -> "-"
            | C.Div -> "/"
            | C.Modulo -> "%"
            | C.Power -> "^"
            | C.And -> "&&"
            | C.Or -> "||"
            | C.Eq -> "=="
            | C.NotEq -> "!="
            | C.Greater -> ">"
            | C.Less -> "<"
            | C.GreaterOrEqual -> ">="
            | C.LessOrEqual -> "<="
        let isBinary (e: C.Expr) =
            match e with
            | C.Binary _ -> true
            | _ -> false
        let left =
            let expr = _writeExpr left
            if isBinary left then $"({expr})" else expr
        let right =
            let expr = _writeExpr right
            if isBinary right then $"({expr})" else expr
        $"{left} {op_string} {right}"
        // if isBinary left || isBinary right then
        //     $"({writeExpression left} {op_string} {writeExpression right})"
        // else
        //     $"{writeExpression left} {op_string} {writeExpression right}"
    | C.Value s ->
        valueToString s
    | C.Ident (i, _) ->
        i.Name
    | C.Call(name, args) ->
        let writeExpression' expr =
            match expr with
            | C.Ident (i, _) ->
                if i.IsThisArgument then
                    i.Name
                else
                    _writeExpr expr
            | _ ->
                _writeExpr expr
        let args = args |> List.map writeExpression'
        let args = System.String.Join(", ", args)
        $"{name}({args})"
    | C.MemberAccess(ident, field) ->
        match ident with
        | C.Ident (i, isValueType) when i.IsThisArgument && not isValueType ->
            $"{i.Name}->{field}"
        | _ ->
            $"{_writeExpr ident}.{field}"
    | C.DerefMemberAccess(ident, field) ->
        match ident with
        | C.Ident (i, _) ->
            $"{i.Name}->{field}"
        | _ ->
            $"({_writeExpr ident})->{field}"
    | C.Expr.Emit s ->
        $"{s}"
    | C.IndexedAccess(source, index) ->
        $"{_writeExpr source}[{_writeExpr index}]"
    | C.TypeCast(``type``, expr) ->
        if (``type``.ToTypeString().Contains "voidptr") then
            for i in 1..20 do
                printfn "%A" ``type``
                printfn "%A" expr
        $"({``type``.ToTypeString()})({_writeExpr expr})"
    | C.SizeOf t ->
        $"sizeof({t.ToTypeString()})"
    | C.Unary (op, expr) ->
        match op with
        | C.Deref ->
            match expr with
            | C.Unary (op, ident) when op = C.Ref ->
                _writeExpr ident
            | _ ->
                $"(*{_writeExpr expr})"
        | C.Ref ->
            $"&{_writeExpr expr}"
        | C.Not ->
            $"!({_writeExpr expr})"
    | C.Expr.ExprAssignment (expr, value) ->
        $"{_writeExpr expr} = {_writeExpr value}"
    | C.Unknown ->
        ""
writeExpression <- _writeExpr
type SourceBuilder with
    member this.AppendStatement(statement: C.Statement) =
        this.AppendBuilder(writeStatement statement)
    member this.AppendStatementInline(statement: C.Statement) =
        // todo: Possible bug from removing \r
        let s = (writeStatement statement).ToString().Replace("\r\n", "\n")
        if s.EndsWith(";\n") then s.Substring(0, s.Length - 2) else s
        |> SourceBuilder.From
        |> this.AppendBuilderInline

 // todo this actually needs to be a random guid or ptr actually. inc ID will cause issues,
 // todo it needs to be instanced per call not per definition
let rootId = ref 0uL
let doRaii (body: C.Statement list) (sb: SourceBuilder) =
    let useRoot = body |> List.exists (fun s -> s.RequiresDestructor.IsSome)
    let requiresDestructor s =
        match s with
        | C.Declaration info -> info.requiresTracking
        | _ -> false
    let idents = body |> List.filter requiresDestructor
    let root =
        if useRoot then
            rootId := rootId.contents + 1uL
            rootId.contents - 1uL
        else
            0uL
//    if useRoot then ()
//        sb.AppendLine($"// void* root_{root} = malloc(sizeof(void*));")
//        sb.AppendLine($"// void* root = malloc(sizeof(void*));")
//        |> ignore
//        sb.AppendLine($"void* root_{root} = (TestC_Module_Root*)TestC_Module_Root_ctor();")
//        sb.AppendLine($"void* root = (TestC_Module_Root*)init_root();")
//        sb.AppendLine($"Runtime_StructList__unsigned_long root = Runtime_StructList_create__unsigned_long();")
//        |> ignore
    for s in body do
        // todo: we also need to handle when a variable is reassigned
        sb.AppendStatement s |> ignore
        // match s.RequiresDestructor with
    for i in idents do
        match i with
        | C.Declaration info ->
            let (C.Ptr t) = info._type
            sb.AppendLine $"Runtime_end_var_scope({info.name}, {t.ToNameString()}_Destructor);"
            |> ignore
        | _ -> ()
        // sb.AppendLine()
        printfn $"{i}"
//        | Some (typeName, varName) ->
//            let ident = C.Ident ({
//                Name = varName; Type = Unchecked.defaultof<_>; IsMutable = false; IsThisArgument = false; IsCompilerGenerated = false; Range = None
//            }, false)
//            sb.AppendLine ($"add_to_root(root_{root}, (void**)&{varName}, &{typeName}_Finalizer);")
//            sb.AppendLine ($"Runtime_add_to_root(&root, (void**)&{varName}, &{typeName}_Finalizer);")
//            |> ignore
//                sb.AppendStatement (C.Expression (C.Call ($"{typeName}__Finalize", [ ident ])))
//                |> ignore
        // | _ ->
            // ()
//    if useRoot then ()
//        sb.AppendLine $"// msg about root {root}"

//        sb
//            .AppendLine($"Runtime_release_root(&root);")

//            .AppendLine($"free(root);")
//            .AppendLine($"release(root_{root});")
//            .AppendLine($"free(root_{root});")
//        |> ignore

//        sb.AppendStatement (C.Statement.Declaration ( {| _type = C.Type.UserDefined |}))

let rec _writeStmt (statement: C.Statement) : SourceBuilder =
    let sb = SourceBuilder()
    match statement with
    | C.Assignment(expr1, expr2) ->
        match expr1 with
        | C.Ident (ident, isValueType) ->
            match ident.Type with
            | DeclaredType(entityRef, genericArgs) when not isValueType -> // when com.Value.GetEntity(entityRef).IsValueType = false ->
//                sb.AppendLine($"Runtime_assign_to_obj((void**)&{writeExpression expr1}, {writeExpression expr2});")
                // todo: move this?
                sb.AppendLine($"{_writeExpr expr1} = {_writeExpr expr2};")
            | _ ->
                sb.AppendLine($"{_writeExpr expr1} = {_writeExpr expr2};")
        | _ ->
            sb.AppendLine($"{_writeExpr expr1} = {_writeExpr expr2};")
    | C.Emit s ->
        sb.AppendText(s)
    | C.Expression expr ->
        match expr with
        | C.Value C.ValueKind.Void -> sb
        | _ -> sb.AppendLine((_writeExpr expr) + ";")
    | C.Return expr ->
        sb.AppendLine($"return {_writeExpr expr};")
    | C.Declaration decl ->
        match decl._type with
        | C.FunctionPtr(types, returnType) ->
            let s = System.String.Join(", ", types |> List.map (fun t -> t.ToTypeString()))
            match decl.value with
            | C.ExprAssignment expr ->
                sb
                    .AppendLine($"{returnType.ToTypeString()} (*{decl.name})({s}) = {_writeExpr expr};")
            | C.StatementAssignment statement ->
                // todo: is this possible? do we need to handle it differently?
                sb
                    .AppendLine($"{returnType.ToTypeString()} (*{decl.name})({s}) = {_writeStmt statement};")
            | C.Default ->
                sb.AppendLine($"{returnType.ToTypeString()} (*{decl.name})({s});")
        | _ ->
            let prefix = "" //decl._type |> function C.Type.UserDefined _ -> "struct " | _ -> ""
            sb.Append prefix |> ignore
            match decl.value with
            | C.Default ->
                // todo: why was this using (*{decl.name})
                sb.AppendLine($"{decl._type.ToTypeString()} {decl.name};")
            | C.ExprAssignment expr ->
                match decl._type with
                | C.UserDefined(fullName, _, entityRef) ->
                    sb.Append($"{__struct}") |> ignore
                | _ -> ()
                sb.AppendLine($"{decl._type.ToTypeString()} {decl.name} = {_writeExpr expr};")
            | C.StatementAssignment statement ->
                let simpleExpr s =
                    match s with
                    | C.Expression expr -> true
                    | _ -> false
                let rec rewrite statements s =
                    match s with
                    | C.Expression expr ->
                        C.Block ((statements |> List.take (statements.Length - 1)) @ [
                            C.Emit $"{decl.name} = {_writeExpr expr};\n"
                            if decl.requiresTracking then
                                C.Emit $"{decl.name}->__refcount++;\n"
                        ])
                    | C.Conditional (guard, ifTrue, ifFalse) ->
                        // todo: nested if => recursion
                        let ifTrue =
                            let start = ifTrue |> List.take (ifTrue.Length - 1)
                            // let returns = C.StatementAssignment (C.Expr.Emit decl.name, List.last ifTrue)

                            // todo: if requiresTracking then increase refCount
                            let returns =
                                if simpleExpr (List.last ifTrue) then
                                    [
                                        C.Emit $"{decl.name} = {List.last ifTrue |> _writeStmt}"
                                        if decl.requiresTracking then
                                            C.Emit $"{decl.name}->__refcount++;"
                                    ]
                                else
                                    [
                                        // C.Emit $"{decl._type.ToTypeString()} {decl.name};"
                                        rewrite [] (List.last ifTrue)
                                    ]
                            start @ returns
                        let ifFalse =
                            // let start = ifFalse |> List.take (ifFalse.Length - 1)
                            // // todo: if requiresTracking then increase refCount
                            // let returns = C.Emit $"{decl.name} = {List.last ifFalse |> _writeStmt}"
                            // start @ [ returns ]
                            let start = ifFalse |> List.take (ifFalse.Length - 1)
                            // let returns = C.StatementAssignment (C.Expr.Emit decl.name, List.last ifTrue)

                            // todo: if requiresTracking then increase refCount
                            let returns =
                                if simpleExpr (List.last ifFalse) then
                                    [
                                        C.Emit $"{decl.name} = {List.last ifFalse |> _writeStmt}"
                                        if decl.requiresTracking then
                                            C.Emit $"{decl.name}->__refcount++;"
                                    ]
                                else
                                    [
                                        // C.Emit $"{decl._type.ToTypeString()} {decl.name};"
                                        rewrite [] (List.last ifFalse)
                                    ]
                            start @ returns
                        C.Block [ C.Conditional (guard, ifTrue, ifFalse) ]
                let statement =
                    match statement with
                    | C.Block statements ->
                        let lastStatement = statements[statements.Length - 1]
                        rewrite statements lastStatement
                        // match lastStatement with
                        // | C.Expression expr ->
                        //     C.Block ((statements |> List.take (statements.Length - 1)) @ [ C.Emit $"{decl.name} = {_writeExpr expr};\n" ])
                        // | C.Conditional (guard, ifTrue, ifFalse) ->
                        // | _ ->
                        //     statement
                    | _ ->
                        statement
                // match decl._type with
                // | C.UserDefined(fullName, _, entityRef) ->
                //     sb.Append($"{__struct}") |> ignore
                // | _ -> ()
                sb
                    .AppendLine($"{decl._type.ToTypeString()} {decl.name};")
                    .AppendStatement statement
    | C.Block statements ->
        sb
            .AppendLine("{")
            .BeginBlock(fun sb ->
//                statements |> List.iter (sb.AppendStatement >> ignore)
                doRaii statements sb
//                for statement in statements do
//                    sb.Append statement
            )
            .AppendLine("}")
    | C.ForLoop(tuple, expr, statement, statements) ->
        sb
            .Append("for (")
            .AppendStatementInline(C.Declaration tuple)
            .Append("; ")
            .Append(_writeExpr expr + "; ")
            .AppendStatementInline(statement)
            .AppendLine(") {")
            .BeginBlock(fun sb ->
//                statements |> List.map (writeStatement >> sb.AppendBuilder) |> ignore
                doRaii statements sb
            )
            .AppendLine("}")
    | C.WhileLoop(expr, statements) ->
        let expr = _writeExpr expr
        sb.AppendLine($"while ({expr}) {{")
            .BeginBlock(fun sb ->
//                for s in statements do
//                    sb.AppendStatement s |> ignore
                doRaii statements sb
            )
            .AppendLine("}")
    | C.Conditional(guard, ifTrue, ifFalse) ->
        let expr = _writeExpr guard
        sb.AppendLine($"if ({expr}) {{")
            .BeginBlock(fun sb ->
//                for s in ifTrue do
//                    sb.AppendStatement s |> ignore
                doRaii ifTrue sb
            )
            .AppendLine("}")
        |> ignore
        if ifFalse.Length > 0 && not (ifFalse.Length = 1 &&
                                      (ifFalse[0] = C.Expression (C.Value C.ValueKind.Void)) ||
                                       ifFalse[0] = (C.Expression << C.Value <| C.ValueKind.Emit "/* UnitConstant */")) then
            sb
                .AppendLine("else {")
                .BeginBlock(fun sb ->
//                    for s in ifFalse do
//                        sb.AppendStatement s |> ignore
                    doRaii ifFalse sb
                )
                .AppendLine("}")
            |> ignore
        sb
    | _ ->
        sb
//    sb
writeStatement <- _writeStmt
let writeStatements (sb: SourceBuilder) (body: C.Statement list) =
    if body.Length > 0 then
//                for s in body |> List.take (body.Length - 1) do // todo: pull declarations ending with $ctor (but later, fix lookup if it's a ref type)
//                    sb.AppendStatement s |> ignore
        match body[body.Length - 1] with
        | C.Return expr ->
            doRaii (body |> List.take (body.Length - 1)) sb
            sb.AppendStatement body[body.Length - 1]
            |> ignore
        | _ ->
//                    sb.AppendStatement body[body.Length - 1]
//                    |> ignore
            doRaii body sb
let writeFunction (sb: SourceBuilder) (f: C.FunctionInfo) =
    sb
        .AppendLine($"{f.return_type.ToTypeString()} {f.id}({writeFunctionArgs f.args}) {{")
        .BeginBlock(fun sb -> writeStatements sb f.body)
        .AppendLine("}")
        .ToString()

let writeStructField (name: string, typ: C.Type) : string =
    $"{typ.ToTypeString()} {name}"

//let writeGenerics (com: Fable.Compiler) (context: Context) : string =
//    let sb = StringBuilder()
//    sb.ToString()
let structCompare (infoA: C.Struct) (infoB: C.Struct) =
    if not (infoA.members |> List.exists (fun (_type, name) -> match _type with | C.Type.UserDefined _ -> true | _ -> false)) then
        -1
    elif not (infoB.members |> List.exists (fun (_type, name) -> match _type with | C.Type.UserDefined _ -> true | _ -> false)) then
        1
    else
        // if A has anything that references B in its chain
        let rec anyMembersAre (members: (C.Type * string) list) (target: string) =
            match members with
            | [] -> false
            | members ->
                let (_type, _) = Seq.head members
                match _type with
                | C.Type.UserDefined (fullName, _, _) ->
                    if target = fullName then
                        true
                    else
                        anyMembersAre (List.tail members) target
                | _ ->
                    anyMembersAre (List.tail members) target
        let aDepends =
            anyMembersAre infoA.members infoB.tag
        if not aDepends then
            -1
        else
            1
let compileFile (_com: Fable.Compiler) (result: {| compiled_module: Map<_,_>; includes: string list |}) : string =
    com := _com
//    System.IO.File.WriteAllText("/home/dave/code/github/Fable/src/RustSample/Program.c.json", Compiler.Json.serialize _module.Values)
    let _sb = StringBuilder()
    // Import includes
    let compiled_module = result.compiled_module
    let includes = [ "stdint.h"; "stdio.h"; "stdlib.h"; "stdbool.h"; "string.h" ]
    for i in includes do
        _sb.AppendLine($"#include <{i}>")
        |> ignore
    for i in result.includes do
        _sb.AppendLine($"#include \"{i}\"")
        |> ignore
    // todo: import "hashmap.h"
//    _sb.AppendLine "#include \"map.h\"" |> ignore
//    _sb.AppendLine "#include \"runtime.h\"" |> ignore
//    _sb.AppendLine("""
//void release(void* root) {}
//void add_to_root(void* root, void* obj, void* finalizer) {}
// """)
//    |> ignore
//    _sb.AppendLine "" |> ignore
//    _sb.Append (writeGenerics com result) |> ignore
//    _sb.AppendLine "" |> ignore
    // Write out structs
    for decl in compiled_module.Values do
        match decl with
        | C.Struct info ->
            _sb.AppendLine($"typedef struct {info.tag} {info.tag};")
            |> ignore
        | _ -> ()
    let sortedValues =
        compiled_module.Values
        |> Seq.sortWith (fun a b ->
            match (a, b) with
            | (C.Struct infoA, C.Struct infoB) -> structCompare infoA infoB
            | (C.Struct info, _) ->
                1
            | (_, C.Struct info) ->
                -1
            | _ ->
                0
        )
    for decl in sortedValues do
        match decl with
        | C.Struct info ->
            _sb
                .AppendLine($"{__struct}{info.tag} {{")
            |> ignore
            for (fieldType, fieldName) in info.members do
                _sb.AppendLine($"    {fieldType.ToTypeString()} {fieldName};")
                |> ignore
            _sb.AppendLine "};"
            |> ignore
            ()
        | _ -> ()
    // Write function signatures
    for decl in compiled_module.Values do
        match decl with
        | C.Function f ->
            _sb.AppendLine($"{f.return_type.ToTypeString()} {f.id}({writeFunctionArgs f.args});")
            |> ignore
        | _ -> ()
    if includes.Length > 0 then
        _sb.AppendLine("") |> ignore
    for f in compiled_module.Values do
        match f with
        | C.Function f ->
            let sb = SourceBuilder()
            let s = writeFunction sb f
            _sb.AppendLine(s) |> ignore
        | C.StaticVar decl ->
            match decl.value with
            | C.ExprAssignment expr ->
                _sb.AppendLine($"{decl._type.ToTypeString()} {decl.name} = {_writeExpr expr};")
                   .AppendLine("")
                |> ignore
            | C.StatementAssignment statement -> () // todo
            | C.Default -> () // todo
        | C.Struct def ->
            ()
        | _ ->
            ()
//        let function_texts = _module |> List.map (writeFunction (SourceBuilder()))
//        let text = System.String.Join("\n", function_texts)
    _sb.ToString()
//        text
type Attribute<'event> =
    { oof: unit }
type Node<'event> =
    { nodes: Node<'event> list; attrs: Attribute<'event> list }
