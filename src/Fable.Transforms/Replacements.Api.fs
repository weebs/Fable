module Fable.Transforms.Replacements.Api

#nowarn "1182"

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms
open Fable.Transforms.Replacements.Util

type ICompiler = FSharp2Fable.IFableCompiler

let curryExprAtRuntime (com: Compiler) arity (expr: Expr) =
    match com.Options.Language with
    // | Rust -> Rust.Replacements.curryExprAtRuntime com arity expr
    // | Python -> Helper.LibCall(com, "Util", "curry", expr.Type, [makeIntConst arity; expr])
    | _ -> Util.curryExprAtRuntime com arity expr

let uncurryExprAtRuntime (com: Compiler) arity (expr: Expr) =
    match com.Options.Language with
    // | Rust -> Rust.Replacements.uncurryExprAtRuntime com expr.Type arity expr
    // | Python -> Helper.LibCall(com, "Util", "uncurry", expr.Type, [makeIntConst arity; expr])
    | _ -> Util.uncurryExprAtRuntime com arity expr

let partialApplyAtRuntime (com: Compiler) t arity (fn: Expr) (args: Expr list) =
    match com.Options.Language with
    // | Rust -> Rust.Replacements.partialApplyAtRuntime com t arity fn args
    // | Python ->
    //     let args = NewArray(ArrayValues args, Any, MutableArray) |> makeValue None
    //     Helper.LibCall(com, "Util", "partialApply", t, [makeIntConst arity; fn; args])
    | _ -> Util.partialApplyAtRuntime com t arity fn args

let tryField (com: ICompiler) returnTyp ownerTyp fieldName =
    match com.Options.Language with
    | Rust -> Rust.Replacements.tryField com returnTyp ownerTyp fieldName
    | Python -> Py.Replacements.tryField com returnTyp ownerTyp fieldName
    | Dart -> Dart.Replacements.tryField com returnTyp ownerTyp fieldName
    | _ -> JS.Replacements.tryField com returnTyp ownerTyp fieldName

let tryBaseConstructor
    (com: ICompiler)
    ctx
    (ent: EntityRef)
    (argTypes: Lazy<Type list>)
    genArgs
    args
    =
    match com.Options.Language with
    | Python ->
        Py.Replacements.tryBaseConstructor com ctx ent argTypes genArgs args
    | Dart ->
        Dart.Replacements.tryBaseConstructor com ctx ent argTypes genArgs args
    | _ -> JS.Replacements.tryBaseConstructor com ctx ent argTypes genArgs args

let makeMethodInfo
    (com: ICompiler)
    r
    (name: string)
    (parameters: (string * Type) list)
    (returnType: Type)
    =
    match com.Options.Language with
    | _ -> JS.Replacements.makeMethodInfo com r name parameters returnType

let tryType (com: ICompiler) (t: Type) =
    match com.Options.Language with
    | Rust -> Rust.Replacements.tryType t
    | Python -> Py.Replacements.tryType t
    | Dart -> Dart.Replacements.tryType t
    | _ -> JS.Replacements.tryType t

let tryCall (com: ICompiler) ctx r t (info: Fable.ReplaceCallInfo) thisArg args =
    match com.Options.Language with
    | Rust -> Rust.Replacements.tryCall com ctx r t info thisArg args
    | Python -> Py.Replacements.tryCall com ctx r t info thisArg args
    | Dart -> Dart.Replacements.tryCall com ctx r t info thisArg args
    | Plugin "C" ->
        let ptrModule = "Microsoft.FSharp.NativeInterop.NativePtrModule"
        match info.CompiledName, info.DeclaringEntityFullName with
        | "Dispose", "System.IDisposable"
        | "UnboxGeneric", "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions"
        | _, "System.Collections.Generic.IEnumerator`1"
        | _, "System.Collections.Generic.IEnumerable`1" ->
            None
        | "GetHashCode", "System.Object" ->
            None
        | "op_Range", "Microsoft.FSharp.Core.Operators" ->
            None
        | "CreateSequence", "Microsoft.FSharp.Core.Operators" ->
            None
        | _, "System.Collections.IEnumerator`1" ->
            None
        | _, "System.Collections.IEnumerator" ->
            None
        | _, "Microsoft.FSharp.Collections.SeqModule" ->
            None
        | ".ctor", "System.Collections.Generic.Dictionary`2" -> None
        | "get_Item", "System.Collections.Generic.Dictionary`2" -> None
        | ".ctor", "System.Collections.Generic.List`1" -> None
        | "Add", "System.Collections.Generic.List`1" -> None
        | "get_Count", "System.Collections.Generic.List`1" -> None
        | "ToInt", "Microsoft.FSharp.Core.Operators" ->
            Some (TypeCast (args[0], t))
        | "GetPointerInlined", _mod when _mod = ptrModule ->
            Helper.LibCall(com, ptrModule, "GetPointerInlined", t, args, ?loc=r)
            |> Some
        | "OfNativeIntInlined", _mod when _mod = ptrModule ->
            Helper.LibCall(com, ptrModule, "OfNativeIntInlined", t, args, ?loc=r)
            |> Some
        | "AllocHGlobal", "System.Runtime.InteropServices.Marshal" ->
            Helper.LibCall(com, info.DeclaringEntityFullName, info.CompiledName, t, args, ?loc=r)
            |> Some
        | "FreeHGlobal", "System.Runtime.InteropServices.Marshal" ->
            Helper.LibCall(com, info.DeclaringEntityFullName, info.CompiledName, t, args, ?loc=r)
            |> Some
        | "Exit", "Microsoft.FSharp.Core.Operators"
        | _, "Microsoft.FSharp.NativeInterop.NativePtrModule" ->
            Helper.LibCall(com, info.DeclaringEntityFullName, info.CompiledName, t, args, ?loc=r)
            |> Some
        | ".ctor", entityName when entityName.StartsWith "Microsoft.FSharp.Core.PrintfFormat" ->
            let replaced = JS.Replacements.tryCall com ctx r t info thisArg args
            replaced
        | "PrintFormatLine", "Microsoft.FSharp.Core.ExtraTopLevelOperators" ->
            let replaced = JS.Replacements.tryCall com ctx r t info thisArg args
            replaced
        | "Substring", "System.String" ->
            let ident = Fable.IdentExpr {
                Name = "Fable." + info.DeclaringEntityFullName + "." + info.CompiledName
                Type = Fable.Unit // todo: create right type
                IsMutable = false
                IsThisArgument = false
                IsCompilerGenerated = false
                Range = None
            }
            let entRef = { FullName = "Fable.System.String"; Path = CoreAssemblyName "TODO: mscorlib" }
            let memberRefInfo = { IsInstance = true; CompiledName = "Substring"; NonCurriedArgTypes = Some info.SignatureArgTypes; Attributes = Seq.empty }
            let callInfo: Fable.CallInfo = {
                ThisArg = thisArg
                Args = args
                SignatureArgTypes = info.SignatureArgTypes
                GenericArgs = info.GenericArgs
                MemberRef = Some (MemberRef (entRef, memberRefInfo))
                Tags = []
            }
            let call = Some (Fable.Call (ident, callInfo, t, r))
            call
            None
            // call
            // Helper.LibCall(com, "Fable." + info.DeclaringEntityFullName, info.CompiledName, t, args, ?loc=r)
            // |> Some
        | _ ->
            let replaced = JS.Replacements.tryCall com ctx r t info thisArg args
            match replaced with
            | None ->
                None
            | _ ->
                // printfn $"Replaced {info.CompiledName} from {info.DeclaringEntityFullName} with"
                // let s = $"%A{replaced}"
                // printfn $"{s}"
                replaced
    | _ -> JS.Replacements.tryCall com ctx r t info thisArg args

let error (com: ICompiler) msg =
    match com.Options.Language with
    | Python -> Py.Replacements.error msg
    | Rust -> Rust.Replacements.error msg
    | Dart -> Dart.Replacements.error msg
    | _ -> JS.Replacements.error msg

let defaultof (com: ICompiler) ctx r typ =
    match com.Options.Language with
    | Rust -> Rust.Replacements.getZero com ctx typ
    | Python -> Py.Replacements.defaultof com ctx r typ
    | Dart -> Dart.Replacements.getZero com ctx typ
    | _ -> JS.Replacements.defaultof com ctx r typ

let createMutablePublicValue (com: ICompiler) value =
    match com.Options.Language with
    | Python -> Py.Replacements.createAtom com value
    | JavaScript
    | TypeScript -> JS.Replacements.createAtom com value
    | Rust
    | Php
    | Plugin _
    | Dart -> value

let getRefCell (com: ICompiler) r typ (expr: Expr) =
    match com.Options.Language with
    | Python -> Py.Replacements.getRefCell com r typ expr
    | Rust -> Rust.Replacements.getRefCell com r typ expr
    | Dart -> Dart.Replacements.getRefCell com r typ expr
    | _ -> JS.Replacements.getRefCell com r typ expr

let setRefCell (com: ICompiler) r (expr: Expr) (value: Expr) =
    match com.Options.Language with
    | Python -> Py.Replacements.setRefCell com r expr value
    | Rust -> Rust.Replacements.setRefCell com r expr value
    | Dart -> Dart.Replacements.setRefCell com r expr value
    | Plugin "C" ->
        Set(expr, SetKind.ValueSet, expr.Type, value, r)
    | _ -> JS.Replacements.setRefCell com r expr value

let makeRefCellFromValue (com: ICompiler) r (value: Expr) =
    match com.Options.Language with
    | Python -> Py.Replacements.makeRefCellFromValue com r value
    | Rust -> Rust.Replacements.makeRefCellFromValue com r value
    | Dart -> Dart.Replacements.makeRefCellFromValue com r value
    | _ -> JS.Replacements.makeRefCellFromValue com r value

let makeRefFromMutableFunc (com: ICompiler) ctx r t (value: Expr) =
    match com.Options.Language with
    | Python -> Py.Replacements.makeRefFromMutableFunc com ctx r t value
    | Rust -> Rust.Replacements.makeRefFromMutableFunc com ctx r t value
    | Dart -> Dart.Replacements.makeRefFromMutableFunc com ctx r t value
    | _ -> JS.Replacements.makeRefFromMutableFunc com ctx r t value

let makeRefFromMutableValue (com: ICompiler) ctx r t (value: Expr) =
    match com.Options.Language with
    | Python -> Py.Replacements.makeRefFromMutableValue com ctx r t value
    | Rust -> Rust.Replacements.makeRefFromMutableValue com ctx r t value
    | Dart -> Dart.Replacements.makeRefFromMutableValue com ctx r t value
    | Plugin "C" ->
        Operation (OperationKind.Unary (UnaryOperator.UnaryAddressOf, value), [], value.Type, r)
    | _ -> JS.Replacements.makeRefFromMutableValue com ctx r t value

let makeRefFromMutableField (com: ICompiler) ctx r t (value: Expr) =
    match com.Options.Language with
    | Python -> Py.Replacements.makeRefFromMutableField com ctx r t value
    | Rust -> Rust.Replacements.makeRefFromMutableField com ctx r t value
    | Dart -> Dart.Replacements.makeRefFromMutableField com ctx r t value
    | _ -> JS.Replacements.makeRefFromMutableField com ctx r t value
