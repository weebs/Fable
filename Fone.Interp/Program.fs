module Fone.Interp.Program

open System
open System.IO
open System.Linq.Expressions
open System.Text
open Avalonia.Controls
open FSharp.Compiler.Interactive.Shell
open Fone.Interp.AST
open Fone.Interp.AST.Typed
open TokenParser
open AST.Unchecked
open Printer
open FParsec
open AST
type ConsoleWriter(?callback: (char -> unit)) =
        inherit IO.TextWriter()

        let print: char -> unit =
            callback |> Option.defaultValue (fun c -> printf $"{c}")
            // ignore
        override this.Write(c: char) =
            // if not ignoreInteractiveOutput then
            print c
        override this.Encoding = Encoding.Default
type Session =
    static member create (outStream: IO.TextWriter, ?errorStream: IO.TextWriter) = task {
        // Initialize output and input stream
        let mutable fsi: FsiEvaluationSession = Unchecked.defaultof<_>

        let sbErr = new StringBuilder()
        let inStream = new IO.StringReader("")
        let outStream =
            outStream //|> Option.defaultWith (fun () -> new IO.StringWriter(sbOut))
        // let errStream = new IO.StringWriter(sbErr)
        // let errStream = new IO.StringWriter(sbErr)
        let errStream =
            // errorStream |> Option.defaultValue (new Text.ConsoleWriter())
            outStream

        // Build command line arguments & start FSI session
        let argv = [| //"/usr/share/dotnet/sdk/6.0.403/FSharp/fsi.dll"
                      "/home/dave/apps/dotnet/dotnet7/sdk/7.0.200/FSharp/fsi.dll"
                       |]

        let allArgs =
            //Array.append argv [| "--quiet" |]
            Array.append argv  [| "--multiemit-" |] //[| "--noninteractive" |]

        let fsiConfig =
            FsiEvaluationSession.GetDefaultConfiguration()

        let fsiSession =
            FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream, collectible=true)
        let evalAsync (f: FsiEvaluationSession -> 'a) =
            task {
                return f fsiSession
            }
        return fsiSession
        // return fsiSession, evalAsync
    }

type SourceExpression =
    | Leaf of Token list
    | Tree of root: Token list * nodes: SourceExpression list
    with
    member this.Flatten =
        match this with
        | Leaf tokens -> Token.Line tokens
        | Tree (root, nodes) ->
            // let flattened = nodes |> List.map _.Flatten |> List.map Token.List
            let flattened = nodes |> List.map _.Flatten
            // root @ flattened
            Token.Line (root @ flattened)


let getSubExpressions (indent: int) (lines: (PrefixWhitespace * Token list) list) =
    let result = lines |> List.takeWhile (fun (ws, _) -> ws.spaces > indent)
    result, lines |> List.skip result.Length
let rec tree (indent: int) (lines: (PrefixWhitespace * Token list) list) =
    match lines with
    | [] -> []
    | (ws, tokens)::moreLines ->
        let subExpressions, remaining = getSubExpressions ws.spaces moreLines
        match remaining with
        | [] ->
            match subExpressions with
            | [] ->
                [ Leaf tokens ]
            | _ ->
                match tokens with
                | n :: Identifier "fn" :: rest -> ()
                | _ -> ()
                [ Tree (tokens, tree indent subExpressions) ]
        | _ ->
            let rest = tree indent remaining
            match subExpressions with
            | [] -> [ Leaf tokens ] @ rest
            | _ ->
                // [ Tree (tokens, subExpressions |> List.map (snd >> Leaf)) ] @ tree indent remaining
                let result = Tree (
                    tokens,
                    tree indent subExpressions
                )
                [ result ] @ tree indent remaining
                // [ Tree (tokens, subExpressions |> List.map (snd >> Leaf)) ] @ tree indent remaining
let parseArg (token: Token) =
    match token with
    | Token.List (Identifier name :: Identifier ":" :: [ Identifier typeName ]) ->
        { Name = name; TypeConstraint = Some typeName }
    | Token.List [ Identifier name ] ->
        { Name = name; TypeConstraint = None }
    | Identifier name ->
        { Name = name; TypeConstraint = None }
    | Token.List [] ->
        { Name = "()"; TypeConstraint = Some "unit" }
    // | _ -> { Name = ""; Type = None }
module rec Parse =
    let parseCallArgs callee (args: Token list) =
        // let f arg =
        //     match arg with
        //     | Line
        args |> List.map parseExpression
    module Data =
        let takeToken token =
            // match token with
            // | Token.Line (start::tail) ->
            //     match start with
            //     | Token.Line rest ->
            //         let next, more = takeToken start
            //         // next, Token.Line (tail @ [ more ])
            //         next, Token.Line (more :: tail)
            //     | _ ->
            //         match tail with
            //         | [ Token.Line rest ] ->
            //             start, Token.Line rest
            //         | [ token ] ->
            //             start, token
            //         | _ ->
            //             start, Token.Line tail
            // | _ -> token, Token.Null
            match token with
            | Token.Line (start::tail) ->
                match start with
                | Token.Line rest ->
                    let next, more = takeToken start
                    // next, Token.Line (tail @ [ more ])
                    next, (more @ tail) // Token.Line (more :: tail)
                | _ ->
                    match tail with
                    | [ Token.Line rest ] ->
                        start, rest //Token.Line rest
                    | [ _ ] ->
                        start, tail
                        // token, [] // token
                    | _ ->
                        start, tail // Token.Line tail
            | _ -> token, [] // Token.Null

        let takeWhileNot t token =
            let rec loop acc token =
                match takeToken token with
                | value, rest when value = t ->
                    acc, [ token ]
                    // acc @ [ token ], rest
                // | value, Token.Null when value <> t ->
                | Token.Line tokens, []
                | Token.List tokens, [] ->
                    // loop acc skipToken (Token.Line tokens)
                    takeWhileNot t (Token.Line tokens)
                | value, [] when value <> t ->
                    failwith "ope"
                | token, rest ->
                    let a, b = loop (acc @ [ token ]) (Token.Line rest)
                    a, b //@ List.tail rest
                    // loop (acc @ [ token ]) rest
            loop [] token
        let skipToken token =
            takeToken token |> snd
        // let takeTokenUntil t token =
        //     match token with
        //     | Token.Line tokens ->
        //         let tokens = [
        //             for token in tokens do
        //                 match token with
        //                 | Token.Line moreTokens ->
        //                     let result = takeTokenUntil t moreTokens
        //
        //                 | _ ->
        //                     if token <> t then
        //                         yield token
        //                 if t = token then
        //         ]
    let parseExpression (token: Token) =
        match token with
        | Token.List _ ->
            ()
        | Token.Line _ ->
            // printfn "%A" token
            ()
        | _ -> ()
        let token =
            match token with
            | Token.Line [ t ] -> t
            | Token.Line ts -> Token.List ts
            | Token.List [ t ] -> t
            | _ -> token
            // token |> function Token.Line ts -> Token.List ts | _ -> token
        match token with
        | Token.Line _ ->
            Ignore
        | Token.List [ t ] -> parseExpression t
        | Token.Line (Token.Identifier "let" :: rest)
        | Token.List (Token.Identifier "let" :: rest) ->
            parseLet (Token.Identifier "let" :: rest)
        | Token.List (Token.Identifier "for" :: rest)
        | Token.Line (Token.Identifier "for" :: rest) ->
            let bindings, rest = rest |> Token.Line |> Data.takeWhileNot (Identifier "in")
            // let rest = rest |> Data.skipToken
            let rest = Token.Line rest |> Data.skipToken
            // let rest = rest |> List.skip 1
            // let range, rest = rest |> Data.takeWhileNot (Identifier "do")
            let range, rest = Token.List rest |> Data.takeWhileNot (Identifier "do")
            let rest = Token.Line rest |> Data.skipToken
            ForLoop (bindings |> List.map parseExpression, parseExpression (Token.List range), rest |> List.map parseExpression |> Sequence)
        | Token.List (Token.Identifier "type" :: Token.Identifier name :: rest)
        | Token.Line (Token.Identifier "type" :: Token.Identifier name :: rest) ->
            let typeArgs, rest = rest |> Token.Line |> Data.takeWhileNot (Identifier "=")
            let rest = Token.Line rest |> Data.skipToken
            match rest with
            | [ Token.Sequence fieldTokens ] ->
                let parseField (field: Token list) =
                    field[0] |> parseExpression, field[2] |> parseExpression
                let fieldTokens =
                    fieldTokens
                    |> List.chunkBySize 3
                    |> List.map parseField
                Let (
                    { Name = name; TypeConstraint = Some "Type" },
                    if typeArgs.Length = 0 then
                        RecordInfo fieldTokens
                    else
                        let args =
                            typeArgs |> List.map parseArg
                            |> List.map (fun arg -> { arg with TypeConstraint = arg.TypeConstraint |> Option.defaultValue "Type" |> Some })
                        NonCurriedLambda (args, RecordInfo fieldTokens)
                )
            | [] -> RecordInfo []
            | _ ->
                failwith "type declaration value must be a sequence of \"ident : type \" items"
        | Token.List (Token.Identifier "fun" :: rest)
        | Token.Line (Token.Identifier "fun" :: rest) ->
            let args, rest =
                let args, rest =
                    rest
                    |> Token.Line
                    |> Data.takeWhileNot (Identifier "->")
                args
                |> List.map parseArg, rest
                // |> List.takeWhile (fun t -> t <> Identifier "->")
                // |> List.map parseArg
            let lambdaBody =
                rest
                // |> (function Token.Line rest -> rest)
                |> Token.Line
                |> Data.skipToken
            let lambdaExpr =
                // |> Token.Line
                match lambdaBody with
                | Token.Line _ :: rest
                | Token.List _ :: rest ->
                    lambdaBody |> List.map parseExpression |> Sequence
                | _ ->
                    Token.Line lambdaBody
                    |> parseExpression
                // |> Sequence
                // |> List.skip 1
                // |> List.skip (args.Length + 1)
                // |> parseLetValue
            NonCurriedLambda (args, lambdaExpr)
        // | Token.List (Token.Line callee::args)
        // | Token.Line (Token.Line callee::args) ->
        | Token.List [ Token.Line callee ]
        | Token.Line [ Token.Line callee ] ->
            let asdf = Data.takeToken (Token.Line callee)
            let asdf2 = Data.skipToken (Token.Line callee)
            let asdf22 = Data.takeToken token
            let asdf222 = Data.skipToken token
            parseExpression (Token.Line callee)
            // if args.Length = 0 then
            //     parseExpression (Token.Line (callee @ args))
            // else
            //     Ignore
        | Token.Line [ expr ] ->
            parseExpression expr
        | Token.List (callee::args)
        | Token.Line (callee::args) ->
            let binaryOps = [ "+-/*^"; ".."; "|>"; "<|" ]
            match args with
            | Identifier op :: otherArgs
                    when
                        (op.Length = 1 && binaryOps[0].Contains op) ||
                        (List.tail binaryOps |> List.exists (fun s -> s.Contains op)) ->
                // todo: do the conversion on all args
                match op with
                | ".." ->
                    Range (parseExpression callee, parseExpression otherArgs[0])
                | _ ->
                    Call (parseExpression (Identifier op), List.map parseExpression (callee::otherArgs))
            | Identifier "<-" :: rest ->
                let value = parseExpression (Token.Line rest)
                Assign (parseExpression callee, value)
            | _ ->
                // let callee = parseExpression callee
                let args = parseCallArgs callee args
                Call (parseExpression callee, args)
                // match callee with
                // | Ignore -> Ignore
                // | _ -> Call (callee, args)
        | Identifier name -> Ident name
        | Line [] | Token.List [] ->
            Expr.UnitConstant
        | Token.Number s -> Expr.Number s
        | Comment _ -> Ignore
        | Token.Array values -> ArrayLiteral (values |> List.map parseExpression)
        | _ ->
            Throw token
    let parseLetValue (tokens: Token list) =
        tokens
        |> List.map parseExpression
        |> function
            | [ expr ] -> expr
            | exprs -> Sequence exprs
    let parseLet (tokens: Token list) =
        let toLetValue (rest: Token list) =
            match rest with
            | Token.Line _ :: _
            | Token.List _ :: _ ->
                parseLetValue rest
                // Sequence (rest |> List.map parseExpression)
            | _ ->
                // todo: Line or List
                if rest.Length = 1 then
                    // Since parseLetValue calls List.map parseExpression
                    // we need to wrap the value
                    parseLetValue [ rest[0] ]
                    // parseExpression (Token.Line rest)
                else
                    parseExpression (Token.List rest)
        match tokens with
        | (Identifier "let")::(Identifier name)::rest ->
            match rest with
            | Identifier ":" :: Identifier argType :: Identifier "=" :: value ->
                Let (
                    { Name = name; TypeConstraint = Some argType },
                    toLetValue value
                    // parseLetValue value
                )
            | Identifier "=" :: value ->
                Let (
                    { Name = name; TypeConstraint = None },
                    toLetValue value
                    // parseExpression (
                    //     if value.Length = 1
                    //     then value[0]
                    //     else Token.List value
                    // )
                )
            | _ -> // is a function declaration
                let args =
                    rest
                    |> List.takeWhile (fun t -> t <> Identifier "=")
                    |> List.map parseArg
                let value = rest |> List.skip (args.Length + 1)
                // let letValue =
                //     match value with
                //     | Token.List foo :: rest
                //     | Token.Line foo :: rest ->
                //         value |> parseLetValue
                //     | _ ->
                //         // Since parseLetValue calls List.map parseExpression
                //         // we need to wrap the value
                //         parseLetValue [ Token.Line value ]
                let value =
                    match value with
                    | Token.Line _ :: _ ->
                        value
                    | _ ->
                        [ Token.Line value ]
                Let (
                    { Name = name; TypeConstraint = None },
                    // NonCurriedLambda (args, toLetValue (Token.Line value))
                    NonCurriedLambda (args, toLetValue value)
                )

        | _ ->
            Throw (Token.List tokens)
type TypedEnv = { constraints: Map<string, string option> }
let usages name expr : obj list =
    []
let solve env name =
    None
let rec typeCheck env expr =
    match expr with
    | NonCurriedLambda (argNames, expression) ->
        // Typed.Lambda (typeCheck { env with constraints = env.constraints.Add(argName, "Erased / *") } expression)
        let env =
            argNames
            |> List.fold (fun env arg -> { env with constraints = env.constraints.Add(arg.Name, arg.TypeConstraint) }) env
        let parameters = argNames |> List.map (fun arg -> solve env arg.Name |> Option.defaultValue (Defined ""))
        let returnType = typeCheck env expression
        Type.Lambda (parameters, returnType)
    | RecordInfo fields ->
        Type.Kind expr
    | _ ->
        Type.Dynamic
    // Typed.Type.Defined
let rec typeCheck_ (env: AST.Env) (expr: Expr) =
    let failwith _ =
        Typed.Error expr
    match expr with
    | Let(argInfo, expression) ->
        let t =
            match argInfo.TypeConstraint with
            | Some t -> Type.Defined t
            | _ -> typeCheck { constraints = Map.empty } expression
        Typed.Let (argInfo.Name, typeCheck_ env expression)
    | Call(callee, args) ->
        let rec loop =
            ()
        match args with
        | [ arg ] ->
            Apply (typeCheck_ env callee, typeCheck_ env arg)
        | _ ->
            typeCheck_ env
                (Call
                     (Call (callee, [ List.head args ]),
                      List.tail args))
            // Typed.Call (typeCheck_ env callee, args |> List.map (typeCheck_ env))
    | Range(from, until) -> failwith "todo"
    | Ident s -> Typed.Ident s
    | Throw (Token.Sequence tokens) ->
        match tokens with
        | field :: Token.Identifier "=" :: value :: rest ->
            // todo: Parse properly
            // todo: List.tupleBySize n
            tokens
            |> List.chunkBySize 3
            |> List.map (fun [ Token.Identifier field; _; value ] ->
                field,
                Parse.parseExpression value
                |> typeCheck_ env
            )
            |> NewRecord
        | _ ->
            Unit
    | Throw token ->
        failwith "todo"
    | Sequence expressions ->
        Typed.Sequence (List.map (typeCheck_ env) expressions)
    // | Expr.Number s -> Double.Parse s |> (int64 >> Num >> Value >> Typed.Constant)
    | Expr.Number s -> Double.Parse s |> (int64 >> Number >> Constant)
    | ForLoop(bindings, range, body) ->
        failwith "todo"
    | Ignore ->
        failwith "todo"
    | Expr.UnitConstant -> Expression.Unit
    | ArrayLiteral expressions ->
        Typed.Array (List.map (typeCheck_ env) expressions)
    | Unchecked.Lambda(arg, body) ->
        failwith "todo"
    | Assign(dest, value) ->
        failwith "todo"
    | NonCurriedLambda(args, body) ->
        let typedEnv =
            args
            |> List.fold (fun env arg -> { constraints = env.constraints.Add(arg.Name, None) }) { constraints = Map.empty }
        let argTypes =
            args
            // |> List.map (_.Name >> solve typedEnv)
            |> List.map _.TypeConstraint
            |> List.map (Option.defaultValue "" >> Type.Defined)
            // |> List.map (Option.defaultValue null)
            // |> List.map Kind
        let names = args |> List.map _.Name
        // Typed.Lambda (argTypes |> List.zip names, typeCheck_ env body)
        match args with
        | [ arg ] ->
            Typed.Lambda ((arg.Name, Type.Dynamic), typeCheck_ env body)
        | arg::remainingArgs ->
            let rest = NonCurriedLambda(remainingArgs, body)
            Typed.Lambda ((arg.Name, Type.Dynamic), typeCheck_ env rest)
    | RecordInfo fields ->
        let fields = [
            for (Ident name, t) in fields do
                let result = typeCheck_ env t
                match result with
                | Expression.Ident n ->
                    yield name, Expression.Ident ("@" + n)
                | _ ->
                    yield name, result
        ]
        MakeRecordConstructor fields
let rec compile_ printfn acc (env: {| locals: Map<_,_> |}) expr : _ -> RuntimeValue =
    let compile = compile_ printfn (expr :: acc)
    match expr with
    | Add (a, b) ->
        let a = compile env a
        let b = compile env b
        fun env ->
            let (Number a) = a env
            let (Number b) = b env
            a + b
            |> Number
    | Typed.Lambda ((arg, t), body) ->
        let body = compile env body
        fun (env: {| locals: Map<_,_> |}) ->
            fun value ->
                let env = {| env with locals = env.locals.Add(arg, value) |}
                let result = body env
                result
            |> box
            |> Compiled

let rec eval_ printfn acc env expr =
    let eval = eval_ printfn (expr :: acc)
    match expr with
    | Typed.Add (a, b) ->
        let _, a = eval env a
        let _, b = eval env b
        match a, b with
        | Number a, Number b -> env, Number (a + b)
        | Float a, Float b -> env, Float (a + b)
        | String a, String b -> env, String (a + b)
    | Typed.Mult (a, b) ->
        let _, a = eval env a
        let _, b = eval env b
        match a, b with
        | Number a, Number b -> env, Number (a * b)
        | Float a, Float b -> env, Float (a * b)
    | Typed.Let (name, value) ->
        let _, result = eval env value
        printfn $"let {name} = %A{result}"
        { env with bindings = env.bindings.Add(name, result) }, RuntimeValue.UnitConstant
    | Typed.Apply (callee, value) ->
        match eval env callee with
        | env, RuntimeValue.Lambda (lambdaEnv, arg, expr) ->
            let _, value = eval env value
            let lambdaEnv = lambdaEnv.Add(arg, value)
            match expr with
            | Expression.Lambda ((s, _), body) ->
                env, RuntimeValue.Lambda (lambdaEnv, s, body)
            | _ ->
                let mutable local_env = env.bindings
                for kv in lambdaEnv do
                    local_env <- local_env.Add (kv.Key, kv.Value)
                let _, result = eval { env with bindings = local_env } expr
                env, result
        | env, RuntimeValue.Type t ->
            let value = eval env value
            env, UnitConstant
        | env, toCall ->
            let value = eval env value
            env, UnitConstant
    | Typed.Call(callee, [ args ]) ->
        let _, fn = eval env callee
        match fn with
        | RuntimeValue.Lambda (lambdaEnv, argName, expr) ->
            let value = eval env args
            let lambdaEnv = lambdaEnv.Add(argName, snd value)
            match expr with
            | Expression.Lambda ((s, _), body) ->
                env, RuntimeValue.Lambda (lambdaEnv, s, body)
            | _ ->
                let mutable local_env = env.bindings
                for kv in lambdaEnv do
                    local_env <- local_env.Add (kv.Key, kv.Value)
                let _, result = eval { env with bindings = local_env } expr
                env, result
                // env, Lambda (lambdaEnv, argName, expr)
            // let env = { env with bindings = env.bindings.Add() }
            // env, UnitConstant
        | RuntimeValue.Type t ->
            match t with
            | Type.Record fields ->
                match args with
                | NewRecord values ->
                    env, Struct [
                        for (name, value) in values do
                            (name, eval env value |> snd)
                    ]
                | _ ->
                    env, UnitConstant
            | _ ->
                env, UnitConstant
        | _ ->
            match args with
            // | Expression.Ident "|>" ->
            //     env, (eval env (Typed.Call (args, [ callee; yield! (List.tail args) ])) |> snd)
            | _ ->
                failwith "todo"
    | Typed.Call (callee, args) ->
        eval env (Typed.Call (Typed.Call (callee, [ List.head args ]), List.tail args))
    | Typed.Constant value ->
        env, value
    | Typed.Unit -> env, RuntimeValue.UnitConstant
    | Typed.Ident s ->
        // let s = if s.StartsWith "@" then s.Substring(1) else s
        if env.bindings.ContainsKey s then
            env, env.bindings[s]
        else
            match s with
            | "ignore" -> env, RuntimeValue.Lambda (Map.empty, "", Expression.Unit)
            | "|>" -> env, RuntimeValue.Lambda (Map.empty, "a", Expression.Lambda (( "b", Type.Dynamic ), Expression.Call (Expression.Ident "b", [ Expression.Ident "a" ])))
            | "@int" -> env, RuntimeValue.Type Type.Int64
            | "+" -> env, RuntimeValue.Lambda (Map.empty, "a", Expression.Lambda (( "b", Type.Dynamic ), Expression.Add (Expression.Ident "a", Expression.Ident "b")))
            | "*" -> env, RuntimeValue.Lambda (Map.empty, "a", Expression.Lambda (( "b", Type.Dynamic ), Expression.Mult (Expression.Ident "a", Expression.Ident "b")))
            | _ when s.StartsWith "@" -> eval env (Expression.Ident (s.Substring(1)))
    | Typed.Lambda(arg, body) ->
        env, Lambda (env.bindings, fst arg, body)
        // match args with
        // | [] -> env, Lambda (Map.empty, "", body)
        // | [ arg ] -> env, Lambda (env.bindings, fst arg, body)
        // | arg::rest -> env, Lambda (env.bindings, fst arg, (Typed.Lambda (rest, body)))
    | MultiMethod(arity, returnType) -> failwith "todo"
    | Typed.Array expressions ->
        env, expressions |> List.toArray |> Array.map (eval env >> snd) |> Array
    | Typed.Sequence expressions ->
        let mutable local_env = env
        for expr in expressions |> List.take (expressions.Length - 1) do
            let env, result = eval local_env expr
            local_env <- env
        env, snd (eval env (List.last expressions))
    | Typed.Error expr ->
        env, RuntimeValue.String (sprintf "%A" expr)
    | MakeRecordConstructor fields ->
        env, Type <| Type.Record [
            for (name, e) in fields do
                match (eval env e |> snd) with
                | Type t -> name, t
        ]
    | NewRecord tuples ->
        env, Struct [
            for (name, value) in tuples do
                name, eval env value |> snd
        ]
    // | _ ->
        // env, RuntimeValue.Unit
let eval printfn env expr = eval_ printfn [] env expr
let compileSource text (printfn: string -> unit) =
    match run (manyTill line eof) text with
    | Success (o, unit, position) ->
        let filteredExpressions =
            o
            |> List.filter (fun (_, items) -> items.Length <> 0)
            |> List.map (fun (ws, items) ->
                ws, items |> List.filter (function Comment _ -> false | _ -> true)
            )
        // let asdf =
            // getSubExpressions 0 filteredExpressions
        let expressionTree =
            tree 0 filteredExpressions

        let exprList =
            expressionTree |> List.map _.Flatten
        // printfn "%A" exprList

        let env = ref { bindings = Map.empty; foo = () }
        for expr in exprList do
            let expr = Parse.parseExpression expr
            let typed = typeCheck_ env.Value expr
            let env', result = eval printfn env.Value typed
            // printfn "%A" expr
            // printfn "%A" typed
            printfn $"%A{result}"
            env.Value <- env'
        let expressions =
            exprList
            |> List.map (fun expr ->
                expr
                |> Parse.parseExpression
                |> fun expr ->
                    // printfn "%A" expr
                    expr
            )
        // let typedExpressions = [
        //     for expr in expressions do
        //         let e = typeCheck_ env.Value expr
        //         match e with
        //         | Typed.Let (name, value) ->
        //             env.Value <- { env.Value with bindings = env.Value.bindings.Add(name, value) }
        //         | _ -> ()
        //         e
        // ]
        // let output =
        //     expressions
        //     |> List.map (fun expr -> expr.AsText 0 |> string)
        //     |> String.concat "\n"
        //     |> _.Replace("\r", "")
        Result.Ok ""
    | Failure(s, parserError, unit) ->
        Result.Error (s, parserError)

// do
// match run (manyTill line eof) Sources.source2 with
// match run (manyTill line eof) Sources.normalCode with
let normal () =
    match compileSource Sources.normalCode (printfn "%s") with
    | Result.Ok output ->
    // | Success(o, unit, position) ->
            // |> List.iter (printfn "%s")
        // let (Result.Ok output) = compileSource Sources.normalCode
        printfn $"{output}"
        let t: System.Threading.Tasks.Task<FsiEvaluationSession> = Session.create (new ConsoleWriter (), new ConsoleWriter())
        printfn "Waiting on F# interactive..."
        // let fsi = Session.create (new StringWriter(StringBuilder()))
        let fsi = t.Result
        let result = fsi.EvalInteractionNonThrowing output
        ()
    | Result.Error (s, parserError) ->
        printfn $"{s}"
        printfn $"%A{parserError}"

let ``type constructor test`` () =
    printfn "%A" (compileSource Sources.normalCode)


module UI =
    open Avalonia
    open Avalonia.Controls
    open Avalonia.Themes.Fluent
    open Elmish
    open Avalonia.FuncUI.Hosts
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Elmish
    open Avalonia.Controls.ApplicationLifetimes

    module Counter =
        open Avalonia.FuncUI.DSL
        type Model = {
            code: string
            messages: string list
        }
        type Message =
            | Start
            | Message of string
        let init () = { code = ""; messages = [] }, []
        let update msg model =
            match msg with
            | Start ->
                let src = Sources.normalCode
                { model with code = src }, Cmd.ofEffect (fun dispatch ->
                    let print s =
                        dispatch (Message s)
                    compileSource src print
                    |> ignore
                )
            | Message s ->
                { model with messages = model.messages @ [ s ] }, []
        let view state dispatch =
            DockPanel.create [
                DockPanel.children [
                    Button.create [
                        Button.content "foo"
                        Button.onClick (fun e -> dispatch Start)
                    ]
                    TextBlock.create [
                        TextBlock.text state.code
                    ]
                    TextBlock.create [
                        TextBlock.text (String.concat "\n" state.messages)
                    ]
                ]
            ]

    type MainWindow() as this =
        inherit HostWindow()
        do
            base.Title <- "Counter Example"
            // base.Icon <- WindowIcon(System.IO.Path.Combine("Assets","Icons", "icon.ico"))
            base.Height <- 400.0
            base.Width <- 400.0

            //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
            //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true
            Elmish.Program.mkProgram Counter.init Counter.update Counter.view
            |> Program.withHost this
            // |> Program.withConsoleTrace
            |> Program.run

    type App() =
        inherit Application()

        override this.Initialize() =
            this.Styles.Add (FluentTheme())
            this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

        override this.OnFrameworkInitializationCompleted() =
            match this.ApplicationLifetime with
            | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
                let mainWindow = MainWindow()
                desktopLifetime.MainWindow <- mainWindow
            | _ -> ()

    module Program =

        [<EntryPoint>]
        let main(args: string[]) =
            AppBuilder
                .Configure<App>()
                .UsePlatformDetect()
                .UseSkia()
                .StartWithClassicDesktopLifetime(args)
