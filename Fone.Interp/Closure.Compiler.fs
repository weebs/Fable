module Fone.Interp.Closure_Compiler

open System.Threading

type Env =
    { bindings: Map<string, obj>; callStack: string list }
    with
    static member Empty = { bindings = Map.empty; callStack = [] }

module TypedAst =
    type Expr =
        | ConstExpr of obj
        | Let of name: string * value: Expr * body: Expr
        | Lambda of arg: string * expr: Expr
        | Multiply of a: Expr * b: Expr
        | Apply of expr: Expr * value: Expr
        | Var of ident: string
module rec Abstract =
    open TypedAst
    let compile (expr: Expr) env : unit -> obj =
        match expr with
        | Let(name, value, body) ->
            let_ name value body env
        | Lambda(arg, expr) ->
            lambda arg expr env
        | Apply(expr, value) ->
            let expr = compile expr env
            let value = compile value env
            fun () ->
                ((expr ()) :?> obj -> obj) (value ())
        | Multiply(a, b) ->
            let a = compile a env
            let b = compile b env
            fun () ->
                let a1 = a ()
                let b1 = b ()
                (a1 :?> int) * (b1 :?> int)
                |> box
        | Var ident ->
            var_ ident env
        | ConstExpr value -> fun () -> value
    let var_ (name: string) (env: Env) =
        fun () ->
            // (env.bindings[name] :?> ThreadLocal<obj ref>).Value.Value
            (env.bindings[name] :?> ThreadLocal<obj>).Value
    let let_ (name: string) value expr env =
        // let variable = ref null
        // let value = compile value env
        // let env = { env with bindings = env.bindings.Add(name, variable) }
        // let _in = compile expr env
        // fun () ->
        //     variable.Value <- value ()
        //     _in ()
        let variable = new ThreadLocal<_>(fun () -> null)
        let value = compile value env
        let env = { env with bindings = env.bindings.Add(name, variable) }
        let _in = compile expr env
        fun () ->
            variable.Value <- value ()
            _in ()
    let lambda (arg: string) expr =
        fun env ->

            // todo: stacklet id?

            // todo: Instead of a ref, get a unique ID
            // todo: and then query a thread-local map for the variable?
            let variable = new ThreadLocal<_>(fun () -> null)
            let env = { env with bindings = env.bindings.Add (arg, variable) }
            // let body = compile expr env
            let body = compile (Let (arg, Var arg, expr)) env
            // todo: the variable is going to get saved across threads
            fun () ->
                fun (arg: obj) ->
                    let oldValue = variable.Value
                    variable.Value <- arg
                    let result = body ()
                    variable.Value <- oldValue
                    result
                |> box
open TypedAst
open Abstract
let test () =
    let multiply_int =
        Lambda ("n", (Lambda ("n2", Multiply (Var "n", Var "n2"))))
    let _44 =
        Apply(Apply(multiply_int, ConstExpr 42), ConstExpr 2)
    let expr = compile multiply_int { bindings = Map.empty; callStack = [] }
    let value = expr () :?> obj -> obj
    let value' = value 2 :?> obj -> obj
    let result = value' 40
    let result2 = (compile _44 Env.Empty) ()
    printfn $"%A{result}"
let mutable n = 0
let rec fix f x =
    n <- n + 1
    if n % 1000 = 0 then
        printfn $"n = {n}"
    f (fix f) x

let factorial recursion n =
    if n % 1000 = 0 then
        printfn $"factorial {n}"
    match n with 0 -> 1 | _ -> n * (recursion (n - 1))
let factorialTest =
    try
        (fix factorial) 100000
        |> ignore
    with error -> printfn "error"
