module Fone.Interp.Program

open System
open System.IO
open System.Text
open FSharp.Compiler.Interactive.Shell
open TokenParser
open AST.Unchecked
open Printer
open FParsec
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
            printfn "%A" token
            // ()
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
                        NonCurriedLambda (typeArgs |> List.map parseArg, RecordInfo fieldTokens)
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
            let binaryOps = [ "+-/*^"; ".." ]
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
            UnitConstant
        | Token.Number s -> Number s
        | Comment _ -> Ignore
        | Array values -> ArrayLiteral (values |> List.map parseExpression)
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
                let letValue =
                    match value with
                    | Token.List foo :: rest
                    | Token.Line foo :: rest ->
                        value |> parseLetValue
                    | _ ->
                        // Since parseLetValue calls List.map parseExpression
                        // we need to wrap the value
                        parseLetValue [ Token.Line value ]
                Let (
                    { Name = name; TypeConstraint = None },
                    NonCurriedLambda (args, toLetValue value)
                )

        | _ ->
            Throw (Token.List tokens)

let t: System.Threading.Tasks.Task<FsiEvaluationSession> = Session.create (new ConsoleWriter (), new ConsoleWriter())
// match run (manyTill line eof) Sources.source2 with
match run (manyTill line eof) Sources.normalCode with
| Success(o, unit, position) ->
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
    printfn "%A" exprList

    let result =
        exprList
        |> List.map (fun expr ->
            expr
            |> Parse.parseExpression
            |> fun expr ->
                printfn "%A" expr
                expr
        )
    let output =
        result
        |> List.map (fun expr -> expr.AsText 0 |> string)
        |> String.concat "\n"
        |> _.Replace("\r", "")
        // |> List.iter (printfn "%s")

    printfn $"{output}"
    printfn "Waiting on F# interactive..."
    // let fsi = Session.create (new StringWriter(StringBuilder()))
    let fsi = t.Result
    let result = fsi.EvalInteractionNonThrowing output
    ()
| Failure(s, parserError, unit) ->
    printfn $"{s}"
    printfn $"%A{parserError}"
