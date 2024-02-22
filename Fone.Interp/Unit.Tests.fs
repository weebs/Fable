module Fone.Interp.Unit_Tests
//type Expr<'T> =
//    Const : 'T -> Expr<'T>
//    Add : Expr<int> -> Expr<int> -> Expr<int>
//    IfThenElse : Expr<bool> -> Expr<'T> -> Expr<'T> -> Expr<'T>
//    App : Expr<'T -> 'S> -> Expr<'T> -> Expr<'S>
//    Lam : (Expr<'T> -> Expr<'S>) -> Expr<'T -> 'S>
//    Fix : Expr<('T -> 'S) -> 'T -> 'S> -> Expr<'T -> 'S>


// instaces of IPatternMatch encode a match expression

// let toEval =


[<AbstractClass>]
type Expr<'T> () =
    abstract Match : IPatternMatch<'T, 'R> -> 'R
and IPatternMatch<'T, 'R> =
    abstract Const : 'T -> 'R
    abstract Add : Expr<int> -> Expr<int> -> 'R
    abstract IfThenElse : Expr<bool> -> Expr<'T> -> Expr<'T> -> 'R
    abstract App<'S> : Expr<'S -> 'T> -> Expr<'S> -> 'R
    abstract Lam<'T1, 'T2> : (Expr<'T1> -> Expr<'T2>) -> 'R
    abstract Fix<'T1, 'T2> : Expr<('T1 -> 'T2) -> 'T1 -> 'T2> -> 'R

// concrete case implementations

type Const<'T>(value : 'T) =
    inherit Expr<'T> ()
    override __.Match (m : IPatternMatch<'T, 'R>) = m.Const value

type Add(left : Expr<int>, right : Expr<int>) =
    inherit Expr<int> ()
    override __.Match (m : IPatternMatch<int, 'R>) = m.Add left right

type IfThenElse<'T>(b : Expr<bool>, l : Expr<'T>, r : Expr<'T>) =
    inherit Expr<'T> ()
    override __.Match (m : IPatternMatch<'T, 'R>) = m.IfThenElse b l r

type App<'T,'S> (f : Expr<'S -> 'T>, x : Expr<'S>) =
    inherit Expr<'T> ()
    override __.Match (m : IPatternMatch<'T, 'R>) = m.App f x

type Lam<'T1,'T2> (f : Expr<'T1> -> Expr<'T2>) =
    inherit Expr<'T1 -> 'T2> ()
    override __.Match (m : IPatternMatch<'T1 -> 'T2, 'R>) = m.Lam f

type Fix<'T, 'S> (f : Expr<('T -> 'S) -> 'T -> 'S>) =
    inherit Expr<'T -> 'S> ()
    override __.Match (m : IPatternMatch<'T -> 'S, 'R>) = m.Fix f

// constructor api

let constant x = Const<_>(x) // :> Expr<_>
let add x y = Add(x,y) // :> Expr<_>
let ifThenElse b l r = IfThenElse<_>(b,l,r) // :> Expr<_>
let app f x = App<_,_>(f,x) // :> Expr<_>
let lam f = Lam<_,_>(f) // :> Expr<_>
let fix f = Fix<_,_>(f) // :> Expr<_>

// let pmatch (pattern : IPatternMatch<'T, 'R>) (x : Expr<'T>) = x.Match pattern



// example : implement evaluator using pattern match

let cast (x : 'T) = x :> obj :?> 'S

let rec pattern<'T> =
    {
        new IPatternMatch<'T, 'T> with
            member __.Const x = x
            member __.Add x y = cast(eval x + eval y)
            member __.IfThenElse b x y = if eval b then eval x else eval y
            member __.App f x = (eval f) (eval x)
            member __.Lam f = cast(eval << f << constant)
            member __.Fix f = cast(eval f (fun x -> eval (fix f) x))
    }

// and eval<'T> (expr : Expr<'T>) : 'T = pmatch pattern<'T> expr
and eval<'T> (expr : Expr<'T>) : 'T = expr.Match pattern

// tests

eval (app (lam (fun b -> ifThenElse b (constant 12) (constant 42))) (constant false))
|> printfn "%A"
let multiply =
    Fix(Lam(fun f ->
        Lam(fun n ->
            IfThenElse (
                constant (eval n = 0),
                Lam (fun _ -> Const 0),
                Lam (fun m ->
                    Add (
                        m,
                        App (
                            App (f, Add (n, Const -1)),
                            m
                        )
                    )
                )
            )
        )
    ))
    // fix (lam(fun f ->
    //     lam(fun n ->
    //         ifThenElse (constant (eval n = 0))
    //             (lam (fun _ -> constant 0))
    //             (lam (fun m -> add m (app (app f (add n (constant -1))) m))))))

eval (app (app multiply (constant 6)) (constant 7))
|> printfn "%A"
type [<Struct>] ValueType =
    | String
    | Integer
    | Float
    | Boolean
type Type =
    | Scalar of ValueType
    | Record of fields: (string * Type) list // fields: Map<string, Type>
    | Union of cases: (string * Type)[]
    | Tuple of fields: Type list
    | Function of arguments: Type list * returnType: Type
    | Type of Type// ?
type Size =
    abstract member Size: int
type DataRepr =
    | Int8 of int8
    | Block of size: int
    | String of string
    | Heap of DataRepr
    interface Size with
        member this.Size =
            match this with
            | Int8 _ -> 1
            | Block size -> size
            | String s -> s.Length
            | Heap h -> (h :> Size).Size
type FieldAccess =
    | Field of offset: int * kind: DataRepr
type FieldAccessor = { Name: string; Method: FieldAccess; Type: Type }
type Value =
    | String of s: string
    | Integer of i: int64
    | Float of f: double
    | Boolean of b: bool
    | Record of fields: Map<string, Value>
    | Union of tag: int * value: Value
    | Tuple of data: Value[]
    | Function of (Value[] -> Value)
    | List of Value list
    | Type of Type
type Expression =
    | Value of Value
    | Call of callee: Expression * args: Expression list
    | Apply of callee: Expression * args: Expression list
    | Lambda of args: string list * expr: Expression
    | Get of value: Expression * field: string
    | TupleGet of value: Expression * field: int
    | UnionCaseGet of value: Expression * case: int
// data Lam :: * -> * where
//   Lift :: a                     -> Lam a        -- ^ lifted value
//   Pair :: Lam a -> Lam b        -> Lam (a, b)   -- ^ product
//   Lam  :: (Lam a -> Lam b)      -> Lam (a -> b) -- ^ lambda abstraction
//   App  :: Lam (a -> b) -> Lam a -> Lam b        -- ^ function application
//   Fix  :: Lam (a -> a)          -> Lam a        -- ^ fixed point
type Erased<'t> = Erased of obj
// type Lam<'t> =
//     | Lambda of (Lam<obj> -> Lam<obj>) * Lam<Erased<obj -> obj>>
// let lambda (f: Lam<'a> -> Lam<'b>) =
//     let f_ (a: Lam<obj>) : Lam<obj> =
//         let arg = a :?> Lam<'a>
//         let result = f arg
//         box result
//     Lambda (Erased f_)
let eval' (expr: Expression) =
    match expr with
    | Call (callee, args) ->
        match callee with
        | Value (Type (Type.Record fields)) ->
            Value (Integer 0)
        | _ ->
            Value (Integer 0)
    | _ ->
        Value (Integer 0)

let SourceRange (a: Type) =
    Type.Record ([
        "start", Scalar ValueType.Integer
        "end", Scalar ValueType.Integer
        // todo: calling type constructors w/ data
        "data", a
    ])
let newRecord (Type.Record t: Type) values =
    // todo: Validate shape
    (Type.Record t, Value.Record values)

type Env = { bindings: Map<string, obj>; callStack: string list }
// type Expression<'t> = Env -> 't
let inline int (n: int) = fun (env: Env) -> box n
let one = int 420
// let inline add (a: Expr<'t>) (b: Expr<'t>) =
    // fun (env: Env) -> (a env) + (b env)
// let _42: Expr<int> = add (int 40) (int 2)
// let inline let_ (name: string) (b: Expr<'t>) body =
let inline let_ (name: string) b body =
    fun (env: Env) ->
        let value = b env
        let nextEnv = { env with bindings = env.bindings.Add (name, value) }
        body nextEnv
let inline value_ value = fun (env: Env) -> value
let inline apply_ value arg =
    fun env ->
        let result: obj -> obj = value env
        result (arg env)
// Lambda (Ident "n", Integer, Call (Ident "*", [ Ident "n"; Number 44 ]))
let inline lambda_ argName expr =
    fun (env: Env) ->
        fun value ->
            let newEnv = { env with bindings = env.bindings.Add (argName, value) }
            expr newEnv
// let inline multiply_ (x: Env -> 'a) (y: Env -> 'a) : Env -> 'a =
let inline multiply_ (x: Env -> obj) (y: Env -> obj) =
    fun (env: Env) ->
        ((x env) :?> int) * ((y env) :?> int)
        |> box
        // (x env) * (y env)
        // |> box
let averageNumsWithFactor (factor: int) (nums: int[]) =
    let mutable n = 0
    for i in 0..nums.Length - 1 do
        n <- nums[i]
    if nums.Length = 0 then float 0
    else float n / float nums.Length
do
    fun o ->
        fun n ->
            // for_
            ()
    |> ignore

let inline ident_<'a> (name: string) = fun (env: Env) -> env.bindings[name] // :?> 'a
let multiply2 =
    // let fn = (*)
    lambda_ "n" (multiply_ (int 2) (ident_ "n"))
    // value_ (fun n -> multiply_ (int 2) (value_ n))
    // lambda_ "n" ()
    // fun (n: int) ->
    //     n * 2
let test () =
    // let expr = let_ "f" (value_ (fun (n: obj) -> string n)) (ident_ "f")
    // let result: obj -> string = (expr { bindings = Map.empty; callStack = [] })
    let env = { bindings = Map.empty; callStack = [] }
    let result = multiply2 env
    let expr = apply_ multiply2 (int 1234)
    let expr = apply_ (fun env -> fun (n: obj) -> ((n :?> int) * 2 |> box)) (int 1234)
    printfn $"result = %A{result}"
    printfn $"call result = %A{expr env}"
    // let_ "fourtytwo" (add (int 40) (int 4)) (ident_ "fourtytwo") <| { bindings = Map.empty }
do
    printfn "%A" (test ())
    test ()

let n =
    let c = constant 2
    let times2 = lam (fun f -> f)
    let mult = lam (fun x -> lam (fun y -> constant (eval x * eval y)))
    let ope = (lam (fun f -> constant (eval f * 2)))
    let mult2 = app mult (constant 2)
    app mult2 (constant 1234)
    // let ope = eval mult
    // ope 420 420

printfn $"%A{eval n}"
