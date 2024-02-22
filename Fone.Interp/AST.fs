module rec Fone.Interp.AST

open System.ComponentModel
open Microsoft.FSharp.Reflection
open TokenParser

type Functor<'a> =
    abstract member Map<'b> : ('a -> 'b) -> Functor<'b>

type 'a List with
    member this.Map (fn: 'a -> 'b) =
        this |> List.map fn

type [<Struct>] IsFunctor<'t>(items: 't[]) =
    interface Functor<'t> with
        member this.Map fn =
            IsFunctor(Array.map fn items)

// let strings = [].Map string
// type Ops =
//     static member map fn (f: _ Functor) =
//         f.Map fn
//     static member map fn (f: _ Functor) =
//         f.Map fn
// open type Ops
// do
    // (IsFunctor([| 1 |])) |> map (fun t -> t.ToString())
module Unchecked =
    type ArgInfo =
        { Name: string; TypeConstraint: string option }
    type Expr =
        | Let of ArgInfo * Expr
        | Call of callee: Expr * args: Expr list
        | Range of from: Expr * until: Expr
        | Ident of string
        | Throw of Token
        | Sequence of Expr list
        | Number of string
        | ForLoop of bindings: Expr list * range: Expr * body: Expr
        | Ignore
        | UnitConstant
        | ArrayLiteral of Expr list
        | Lambda of arg: ArgInfo * body: Expr
        | Assign of dest: Expr * value: Expr
        | NonCurriedLambda of args: ArgInfo list * body: Expr
        | RecordInfo of fields: (Expr * Expr) list



type Type =
    | Dynamic
    | Defined of string
    | Int64
    | Lambda of Type list * Type
    | Value of Value
    | Kind of TypeDesc
    | Record of (string * Type) list
and Value =
    | Num of int64
    | Array of Value[]
and RuntimeValue =
    | Number of int64
    | Float of double
    | String of string
    | Array of RuntimeValue[]
    | UnitConstant
    | Struct of (string * RuntimeValue) list
    | Type of Type
    | TypeConstructor of obj
    | Lambda of env: Map<string, RuntimeValue> * argName: string * body: Typed.Expression
    | Compiled of obj
and TypeDesc =
    obj
module Typed =
    type Expression =
        | Call of callee: Expression * args: Expression list
        // | Constant of Type
        | Constant of RuntimeValue
        | NewRecord of (string * Expression) list
        | MakeRecordConstructor of (string * Expression) list
        | Unit
        | Let of name: string * value: Expression // * body: Expression
        | Ident of string
        | Add of Expression * Expression
        | Mult of Expression * Expression
        // | Lambda of args: (string * Type) list * body: Expression
        | Lambda of arg: (string * Type) * body: Expression
        | MultiMethod of arity: int * returnType: Type
        | Array of Expression list
        | Sequence of Expression list
        | Error of Unchecked.Expr

type Var = { Type: System.Type; mutable Value: obj }

type Env = {
    bindings: Map<string, RuntimeValue>
    foo: unit
}
