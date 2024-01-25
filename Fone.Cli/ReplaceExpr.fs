module rec Fable.C.ReplaceExpr

open System.Text
open System.Threading
open Fable.C.Helpers

open System
open System.Collections
open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.C.AST
open Fable.Transforms.FSharp2Fable
open Fable.C.C99Compiler

module Const =
//    let emitType = "Fable.Interop.EmitType"
    let emitType = "Fable.Core.C.EmitTypeAttribute"
    let byrefType = "Microsoft.FSharp.Core.byref`1"
    let byrefType2 = "Microsoft.FSharp.Core.byref`2"

let replaceInputRecordIdent (expr: Expr) (original: string) (replacement: Expr) =
    expr |> walkExprInPlace (fun e ->
        match e with
        | IdentExpr ident when ident.Name = original -> replacement
        | IdentExpr ident1 -> e
        | Get(IdentExpr ident, FieldGet fieldInfo, ``type``, sourceLocationOption) when ident.Name = original && fieldInfo.Name = "contents" ->
            replacement
        | _ -> e
    )
let replaceInputRecord (e: Expr) =
//    expr |> walkExprInPlace (fun e ->
        match e with
        | Let(originalIdent, originalValue, originalBody) ->
            match originalValue with
            | Let(ident, Call(Import(importInfo, _, _), callInfo, ``type``, sourceLocationOption), body)
                    when ident.Name.StartsWith("inputRecord") && importInfo.Selector = "FSharpRef" && importInfo.Path.EndsWith("/Types.js") ->
                Let(originalIdent, (walkExprInPlace (fun e -> replaceInputRecordIdent e ident.Name callInfo.Args.[0]) body), originalBody)
            | _ -> e
        | _ -> e
//    )

let byrefFullName = "Microsoft.FSharp.Core.byref`2"
let resolveByRefExpr (expr: Expr) : Expr =
    match expr with
    | IdentExpr ident when typeFullName ident.Type = byrefFullName -> IdentExpr { ident with Type = ident.Type.Generics.[0] }
    | _ ->
        match expr.Type with
        | DeclaredType(entityRef, genericArgs) when entityRef.FullName = byrefFullName ->
            expr
        | _ -> expr
let isEmitType (t: Type) =
    match t with
    | DeclaredType(entityRef, genericArgs) ->
        let ent = database.contents.TryGetEntity(entityRef)
        match ent with
        | Some ent ->
            let attributes = ent.Attributes
            attributes |> Seq.tryFind (fun attribute -> attribute.Entity.FullName = Const.emitType) |> Option.map (fun a ->
                match a.ConstructorArgs.[0] with
                | :? string as name -> name
                | _ -> ""
            )
        | _ -> None
    | _ -> None
let replaceEmitTypeCallArgs (expr: Expr) =
    expr |> walkExprInPlace (fun e ->
        match e with
        | Call(callee, callInfo, ``type``, sourceLocationOption) ->
            let args = callInfo.Args |> List.map (fun arg ->
                match isEmitType arg.Type with
                | Some typeName ->
                    TypeCast(arg, DeclaredType ({ FullName = typeName; Path = CoreAssemblyName "EmitType" }, []))
                | _ -> arg
            )
            //Call(callee, { callInfo with Args = args }, ``type``, sourceLocationOption)
            e
        | _ -> e
    )
let rec replaceEmptyDelegatesAndLambdas (e: Expr) =
    match e with
    | Lambda (ident, body, name) ->
        let ident, body = unwrapLambda ident body
        match Query.emptyDelegate ident body with
        | Some (callee, typ, info, r) -> callee
        | _ -> e
    | Delegate (args, body, name, tags) ->
        match Query.emptyDelegate args body with
        | Some (callee, typ, info, r) -> callee
        | _ -> e
    | _ -> e
let rec replaceTmdsCalls (e: Expr) =
    let inline step title =
        printfn $"{title}: Enter to step..."
        // Console.ReadLine() |> ignore
    match e with
    // | Call (callee, info, typ, range) when (sprintf "%A" e).Contains "LibC" ->
        //Console.WriteLine (Print.printExpr 2 e)
        // ()
    // | Call(IdentExpr ident, info, typ, locationOption) when ident.Name.StartsWith("Tmds_Linux") && ident.Name.Contains "op_Implicit" ->
    //     // Emit({ Macro = ident.DisplayName; IsStatement = false; CallInfo = info  }, typ, locationOption)
    //     info.Args[0]
        // Console.WriteLine (Print.printExpr 2 e)
//            Call(IdentExpr { ident with Name = ident.DisplayName }, info, typ, locationOption)
    // | Call(IdentExpr ident, info, typ, locationOption) when
    //         ident.Name.StartsWith("Tmds_Linux_LibC_get_") -> //&& ident.Name.Contains "op_Implicit" ->
    //     let name = ident.Name
    //     printfn $"\n\n{name}\n\n"
    //     e
    | Call(IdentExpr ident, info, typ, range) when
            ident.Name.StartsWith("Tmds_Linux") -> //&& ident.Name.Contains "op_Implicit" ->
            // ident.Name.Contains "op_Implicit" ->
        let name = ident.Name
        if name.Contains "op_Implicit" then
            // todo: Not sure why this also needs walkExprInPlace but _get_ breaks sometimes otherwise
            info.Args[0] |> walkExprInPlace replacements
        elif name.StartsWith "Tmds_Linux_LibC_get_" then
            // Console.WriteLine (Print.printExpr 2 e)
            step "_get_"
            let name = ident.Name.Substring("Tmds_Linux_LibC_get_".Length)
            IdentExpr {
                Name = name
                Type = e.Type
                IsMutable = false
                IsThisArgument = false
                IsCompilerGenerated = true
                Range = range
            }
    // | Call(IdentExpr ident, info, typ, locationOption) when ident.Name.StartsWith("Tmds_Linux_LibC") ->
    //     Emit({ Macro = ident.DisplayName; IsStatement = false; CallInfo = info  }, typ, locationOption)
        elif ident.Name.StartsWith "Tmds_Linux_" &&
             ident.Name.Contains "__set_" &&
             info.ThisArg.IsSome then
            let fieldName =
                let endofstring = (ident.Name.Split ("__set_".ToCharArray()))[1]
                endofstring.Substring(0, endofstring.LastIndexOf "_")
            Set (info.ThisArg.Value, FieldSet fieldName, info.Args[0].Type, walkExprInPlace replacements info.Args[0], range)
        elif ident.Name.StartsWith "Tmds_Linux_LibC" then
            step "Tmds_Linux_LibC"
            // Console.WriteLine (Print.printObj 0 info)
            let info = { info with Args = info.Args |> List.map (walkExprInPlace replacements) }
            Emit({ Macro = ident.DisplayName; IsStatement = false; CallInfo = info }, typ, range)
        else
            step "ELSE"
            printfn "========================== found expr =================================="
            Console.WriteLine("idents name is: " + name)
            Console.WriteLine (ident.Name)
            Console.WriteLine (Print.printExpr 2 e)
            e
    | _ ->
        e
let replaceIdentUsage (name: string) replacement (e: Expr) =
    match e with
    | IdentExpr ident when ident.Name = name -> replacement
    | _ -> e

let replaceCopyOfStruct (e: Expr) =
    match e with
    | Let(ident, value, body) when ident.Name.StartsWith "copyOfStruct" ->
        if ident.Name <> "copyOfStruct" then
            ()
        body |> walkExprInPlace (replaceIdentUsage ident.Name value)
    | _ -> e

let replaceByrefContents (e: Expr) =
//    expr |> walkExprInPlace (fun e ->
        let transformArgs (argType: Type) (arg: Expr) : Expr =
            match arg, arg.Type, argType with
            | Get(IdentExpr ident, FieldGet fieldInfo, ``type``, sourceLocationOption),
              DeclaredType(argRef, _),
              DeclaredType(entityRef, _)
                    when entityRef.FullName = byrefFullName && fieldInfo.Name = "contents" && argRef.FullName = byrefFullName && entityRef.FullName = byrefFullName ->
                IdentExpr ident
            | _ -> arg
        match e with
        | Set(expr, kind, _type, value, _sourceLocationOption) ->
            match kind with
            | ExprSet (Value (StringConstant string, _))
                    when string = "contents" && typeFullName expr.Type = "Microsoft.FSharp.Core.byref`2" ->
                let expr = resolveByRefExpr expr
                //Set(Operation(Unary (UnaryOperator.UnaryDeref, expr), [], expr.Type, _sourceLocationOption), ValueSet, _type, value, _sourceLocationOption)
                Set(expr, ValueSet, _type, value, _sourceLocationOption)
//                 Set(expr, ValueSet, _type, value, _sourceLocationOption)
            | _ -> e
        | Get(expr, FieldGet fieldInfo, _type, sourceLocationOption)
                when fieldInfo.Name = "contents" && typeFullName expr.Type = "Microsoft.FSharp.Core.byref`2" ->
            let expr = resolveByRefExpr expr
            expr
//            Operation(Unary (UnaryOperator.UnaryDeref, expr), _type.Generics.[0], sourceLocationOption)
        | Get(Call(Import(importInfo, import_type, sourceLocationOption), callInfo, call_type, _), FieldGet fieldInfo, typ, range) ->
            if importInfo.Selector = "FSharpRef" then
                Get(callInfo.Args.[0], FieldGet fieldInfo, typ, range)
            else
                e
        | Get(expr, FieldGet fieldInfo, _type, sourceLocationOption)
                when fieldInfo.Name = "contents" ->
            // todo: This could cause a bug for types that have a field named "contents"
            expr
        | Call(callee, callInfo, ``type``, sourceLocationOption) ->
            match callInfo.MemberRef with
            | Some (MemberRef(declaringEntity, memberRefInfo)) when memberRefInfo.CompiledName.Contains("FSharpRef") ->
                e
            | Some (MemberRef(declaringEntity, memberRefInfo)) ->
                if memberRefInfo.NonCurriedArgTypes.IsSome then
                    let args = List.map2 transformArgs memberRefInfo.NonCurriedArgTypes.Value callInfo.Args
                    Call(callee, { callInfo with Args = args }, ``type``, sourceLocationOption)
                else
                    e
            | _ -> e
        | _ -> e
//    )
let replacements =
    (replaceTmdsCalls >> replaceEmptyDelegatesAndLambdas >> replaceByrefContents >> replaceInputRecord >> replaceCopyOfStruct)
