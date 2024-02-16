module Fone.Interp.Program

open TokenParser
open AST.Unchecked
open Printer
open FParsec

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
                    | [ token ] ->
                        start, [] // token
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
                |> Token.Line
                |> parseExpression
                // |> Sequence
                // |> List.skip 1
                // |> List.skip (args.Length + 1)
                // |> parseLetValue
            NonCurriedLambda (args, lambdaBody)
        | Token.List (Token.Line callee::args)
        | Token.Line (Token.Line callee::args) ->
            let asdf = Data.takeToken (Token.Line callee)
            let asdf2 = Data.skipToken (Token.Line callee)
            let asdf22 = Data.takeToken token
            let asdf222 = Data.skipToken token
            parseExpression (Token.Line (callee @ args))
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
        match tokens with
        | (Identifier "let")::(Identifier name)::rest ->
            match rest with
            | Identifier ":" :: Identifier argType :: Identifier "=" :: value ->
                Let (
                    { Name = name; TypeConstraint = Some argType },
                    parseLetValue value
                )
            | Identifier "=" :: value ->
                Let (
                    { Name = name; TypeConstraint = None },
                    parseExpression (
                        if value.Length = 1
                        then value[0]
                        else Token.List value
                    )
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
                    NonCurriedLambda (args, letValue)
                )

        | _ ->
            Throw (Token.List tokens)

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
        |> List.iter (printfn "%s")
    // let result1 = treeify o
    // let result = collectExpressions2 0 o
    // let asdf = collectExpressions exprs
    ()
| Failure(s, parserError, unit) ->
    printfn $"{s}"
    printfn $"%A{parserError}"
