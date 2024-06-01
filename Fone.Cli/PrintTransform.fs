module Fable.C.PrintTransform

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.C.AST
open Helpers
type Generics = (string * Type) list
let pullArgAndType ctx transformType transformExpr transformValueKind (generics: Generics) (arg: Expr) : (C.Type * C.Expr) =
    match arg with
    | TypeCast(expr, _type) ->
        transformType ctx generics (expr.Type), (transformExpr ctx generics expr)
    | Value(valueKind, _sourceLocationOption) ->
        transformType ctx generics valueKind.Type, C.Value (transformValueKind ctx generics valueKind)
    | _ -> failwithf "expected typecast for printf args %A" arg

let format (ctx: C99Compiler.Context) transformType transformExpr transformValueKind (generics: (string * Type) list) (callInfo: CallInfo) =
    let args = callInfo.Args |> List.skip 1 |> List.map (pullArgAndType ctx transformType transformExpr transformValueKind generics)
//                let formatted_string = callArgs[0]
    if args.Length = 0 then
        let args = transformExpr ctx generics callInfo.Args[0]
        match args with
        | C.Value (C.CStr s) ->
            C.Call ("printf", [ C.Value (C.CStr (s + "\\n")) ])
        | _ ->
            C.Call ("printf", [ (transformExpr ctx generics) callInfo.Args[0] ])
    else
        let result = transformExpr ctx generics callInfo.Args[0]
        let mutable (C.Value (C.CStr s)) = result
        for i in 0..(args.Length - 1) do
            s <- s.Replace($"{{{i}}}", (fst args[i]).PrintfType)
        C.Call ("printf", (C.Value (C.CStr (s + "\\n"))) :: (args |> List.map snd))

let toConsole (context: C99Compiler.Context) (com: Type.ICompiler) transformType transformExpr transformValueKind generics (callInfo: CallInfo) : C.Expr =
    let rec transformPrintArg (arg: Expr) : C.Expr list =
        match arg with
        | Emit(emitInfo, _type, _sourceLocationOption) ->
            let mutable s = emitInfo.Macro.Replace("`", "") + "\\n"
            let args: (C.Type * C.Expr) list = emitInfo.CallInfo.Args |> List.map (pullArgAndType context transformType transformExpr transformValueKind generics)
            for i in 0..(emitInfo.CallInfo.Args.Length - 1) do
                #if !FABLE_COMPILER
                s <- s.Replace($"${{${i}}}", (fst args[i]).PrintfType)
                #else
                ()
                #endif
            (C.Value (C.CStr <| s + "\\n")) :: (args |> List.map snd)
        | TypeCast(expr, _type) ->
            transformPrintArg expr
        | Value(valueKind, sourceLocationOption) ->
            match valueKind with
            | StringTemplate(exprOption, parts, values) ->
                let mutable s = ""
                // Parts is the sections of the string between {}'s, so values.Length = parts.Length - 1
                // ex: printfn $"hello {1} to {2} world!" =>
                    // parts = [ "hello "; " to "; " world!" ]
                    // values = [ 1; 2 ]
                let rec transformValue v =
                    match v with
                    | TypeCast(expr, Any) -> transformValue expr
                    | _ ->
                        match v.Type with
                        | DeclaredType(entityRef, genericArgs) ->
                            match com.TryGetEntity(entityRef) with
                            | Some ent ->
                                let fields = ent.FSharpFields
                                Error (v, fields)
                            | _ ->
                                Error (v, [])
                        | _ -> Ok v
                let values = [ // Fable will show printfn values as type casts to Any, which we remove here
                    for v in values do
                        transformValue v
//                        match v with
//                        | TypeCast(expr, Any) -> yield expr
//                        | _ -> yield v
                ]
                for i in 0..(parts.Length - 1) do
                    if i > 0 then
                        match values.[i - 1] with
                        | Ok v ->
//                        if values.[i - 1].Length = 1 then
                            s <- s + (v.Type |> transformType context generics).PrintfType
                        | Error (v, fields) ->
                            let printfComponents = System.String.Join("; ", fields |> List.map (fun f ->
                                $"{f.Name} = {(transformType context generics f.FieldType).PrintfType}"))
                            s <- s + $"{(transformType context generics v.Type).ToTypeString()} {{ {printfComponents} }}"
                    s <- s + parts.[i]
                let newValues = values |> List.collect (fun v ->
                    match v with
                    | Ok v when v.Type = String ->
                        let fieldInfo = {
                            Name = "data"
                            FieldType = Some String
                            IsMutable = false
                            MaybeCalculated = true
                            Tags = []
                        }
                        [ Fable.Get (v, GetKind.FieldGet fieldInfo, v.Type, v.Range) ]
                    | Ok v -> [ v ]
                    | Error (v, fields) ->
                        fields |> List.map (fun f ->
                            let fieldInfo: FieldInfo = { Name = f.Name; FieldType = Some f.FieldType; IsMutable = f.IsMutable; MaybeCalculated = false; Tags = [] }
                            Get (v, GetKind.FieldGet fieldInfo, f.FieldType, None)
                        )
                )
                let s = s.Replace("\n", "\\n")
                C.Value (C.CStr <| s + "\\n") :: (newValues |> List.map (transformExpr context generics))
            | StringConstant s ->
                [ C.DerefMemberAccess (
                    C.Value <| transformValueKind context generics valueKind,
                    "data") ]
            | _ ->
                [ C.Value <| transformValueKind context generics valueKind ]
        | Call(Import(importInfo, _type, _), callInfo, callType, _) when importInfo.Selector = "printf" ->
            let transformArg arg =
                match arg with
                | Fable.Value (StringConstant c, r) ->
                    C.Value (C.CStr c)
                    // C.DerefMemberAccess (
                    //     C.Value <| transformValueKind context generics (StringConstant c),
                    //     "data")
                | _ -> transformExpr context generics arg
            let args = callInfo.Args |> List.map transformArg // (transformExpr context generics)
            match args[0] with
            | C.Value (C.CStr s) ->
                let s = s.Replace("\n", "\\n")
                (C.Value (C.CStr <| s + "\\n")) :: (List.tail args)
            | _ -> args
        | _ -> failwithf "toConsole %A" arg
    let result = callInfo.Args |> List.collect transformPrintArg
    C.Call ("printf", result)
