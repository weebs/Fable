// For more information see https://aka.ms/fsharp-console-apps
open System.Xml.Schema
open FParsec
open FParsec.CharParsers
let x , y = 1 , 2
// let takes3 x y z : int = x + y + z
// let print n =
//     // printfn $"{n}"
//     ()
// do
//     // print
//     //     takes3 1 2 3 // error
//     print
//         (takes3 1 2 3) // fine
//     let n =
//         takes3
//             1
//             2 3 // fine
//     let n2 =
//         takes3 1
//             2 3
//     print n
//     let f =
//         takes3 1
//           2
//         3
//     print f
type PrefixWhitespace = { tabs: int; spaces: int }
type Token =
    | Identifier of string
    | AnnotatedIdentifier of Token * Token
    | Whitespace of tabs: int * spaces: int
    | Number of string
    | List of Token list
    | Line of Token list
    | Null
    | Sequence of Token list
    | Comment of string
    | Array of Token list
type Expr =
    | Let of {| ident: string; value: Expr; body: Expr |}
    | Call of {| callee: Expr; args: Expr list |}
// let ``let`` =
//     pstring "let"
let getWhitespace =
    manyChars (anyOf (List.map char [ " "; "\t"; ])) // "\n"; "\r" ]))
    |>> fun s ->
        let spaces = s |> Seq.filter (fun c -> c = char " ") |> Seq.length
        let tabs = s |> Seq.filter (fun c -> c = char "\t") |> Seq.length
        { tabs = tabs; spaces = spaces }
        // Whitespace (tabs, spaces)
let expr, exprRef = createParserForwardedToRef ()
let ident =
    pstring ".." <|> pstring "::" <|> pstring ":" <|>
    many1Chars (noneOf (" " + ";:" + "{}[]()" + "\\\t\r\n"))
    // many1Chars (noneOf (List.map char [ "{"; "}"; ";"; "["; "]"; "("; ")"; ":"; " "; "\t"; "\n"; "\r"; ]))
    // many1 (noneOf (List.map char [ " "; "\t"; "\n"; "\r"; ]))
    .>> getWhitespace
    |>> fun id ->
        Identifier id
let number =
    numberLiteral NumberLiteralOptions.None ""
    |>> fun literal ->
        Number literal.String
let list =
    pchar '(' >>. many1 expr .>> pchar ')'
    |>> Token.List
let array =
    // pchar '[' .>> getWhitespace >>.
    // many1 (expr .>> (optional (pchar ';')) .>> getWhitespace) .>>
    // getWhitespace .>> pchar ']' .>>
    // getWhitespace
    // |>> Array
    parse {
        do! pchar '[' .>> getWhitespace |>> ignore
        let! inner = many1 (expr .>> (optional (pchar ';')) .>> getWhitespace)
        do! getWhitespace .>> pchar ']' .>> getWhitespace |>> ignore
        return (Array inner)
    }
let seq =
    pchar '{' .>> getWhitespace >>.
    optional (newline .>> getWhitespace) >>.
    many1 (
        expr .>> optional (pchar ';') .>> optional newline .>> getWhitespace
    ) .>>
    getWhitespace .>> pchar '}' .>> getWhitespace
    |>> Sequence
let newlineOrEof =
    ((newline |>> ignore) <|> eof) // .>> skipRestOfLine true
    // >>% []
    |>> fun result ->
        []

let foo (x: int) y =
 x
 + y

let comment =
    pstring "//" >>. restOfLine true
    |>> Comment
let emptyList =
    pstring "()" >>% Token.List []
let manyExprs =
    many1 (expr .>> getWhitespace)
    .>> skipRestOfLine true
    |>> fun result ->
        result
do
    exprRef.Value <-
        emptyList <|> seq <|> comment <|> array <|> list <|> number <|> ident
let line =
    getWhitespace .>>. (
        newlineOrEof
        <|> manyExprs
    )
type UncheckedExpr =
    | Ident of string
    | List of UncheckedExpr list
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
let fn =
    for i
     in 1..10
     do
        printfn ""
        printfn ""
    fun
     ()
      ->
          ()
// let f2 = fun ()
//   ->
//     printfn n
let lambdas = """
let main () =
    let n = string 0
    let printfn (s: string) = System.Console.WriteLine s
    let f = fun () ->
        printfn n
    let f2 = fun ()
        ->
            printfn n
    0
    let f2 =
        fun () ->
            printfn n
    0
"""
let normalCode = """
type SourceRange a = {
    start: int; end: int;
    data: a
}
let add x y = x + y
let mul x y =
    x * y
let main () =
    let n = add 2 3
    let values = [ 1; 2; 3; 4; ]
    let n2 =
        mul 4 20
    let f = fun () ->
        printfn n
    let f2 = fun ()
        ->
            printfn n
    0
    let f2 =
        fun () ->
            printfn n
    0
    for i
     in 1..10
     do
        printfn i
        printfn i
    420
"""
let source = """
let foo (x: int) y =
  let asdf () =
    ()
  let fn =
      fun () ->
        fun () ->
            n <- n + 1
            n * 2
"""
let source2 = """
let foo (x: int) y =
  let asdf () =
    ()
  let fn =
      fun () ->
        fun () ->
            n <- n + 1
            n * 2
  let add3 x y z = x + y + z
  // add3 1
    // 2 3
  add3 1 2 3
  let nums = [ 1; 2; 3; 4 ]
  let n =
    a + c + d + e + f
    x + y
  n + x + y
  let n : int = 0
  foo
    foo x
     y z
     """
type ArgInfo =
    { Name: string; TypeConstraint: string option }
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
    let inline (*) (s: string) (n: int) =
        [| for i in 1..n do yield s |] |> String.concat ""
    type Expression =
        | Let of ArgInfo * Expression
        | Call of callee: Expression * args: Expression list
        | Ident of string
        | Throw of Token
        | Sequence of Expression list
        | Number of string
        | ForLoop of bindings: Expression list * range: Expression * body: Expression
        | Ignore
        | UnitConstant
        | ArrayLiteral of Expression list
        | Lambda of arg: ArgInfo * body: Expression
        | Assign of dest: Expression * value: Expression
        | NonCurriedLambda of args: ArgInfo list * body: Expression
        | RecordInfo of fields: (Expression * Expression) list
        member this.AsText (depth: int) =
            match this with
            | Call(callee, args) ->
                let callee, args =
                    match callee with
                    | Ident "+"
                    | Ident "-"
                    | Ident "*"
                    | Ident ".." -> List.head args, callee :: List.tail args
                    | _ -> callee, args
                let argsText =
                    args
                    |> List.map (fun arg -> arg.AsText 0)
                    // |> String.concat (" " * 4)
                    |> String.concat " "
                $"{callee.AsText depth} {argsText}"
            | Ident s -> s
            | Throw token -> failwith "todo"
            | Sequence expressions ->
                expressions
                |> List.map (fun expr -> expr.AsText 0)
                |> String.concat ("\n" + (" " * (depth + depth + depth + depth)))
            | Number s -> s
            | ForLoop(bindings, range, body) ->
                let bindingsText =
                    bindings
                    |> List.map (fun binding -> binding.AsText depth)
                    |> String.concat ", "
                $"for {bindingsText} in {range.AsText depth} do\n{body.AsText (depth + 1)}"
            | Ignore -> failwith "todo"
            | UnitConstant -> "()"
            | ArrayLiteral expressions ->
                let expressionsText =
                    expressions |> List.map (fun expr -> expr.AsText depth) |> String.concat "; "
                $"[| {expressionsText} |]"
            | Lambda(arg, body) -> failwith "todo"
            | Assign(dest, value) ->
                $"{dest.AsText} <- {value.AsText}"
            // | NonCurriedLambda(args, body) -> failwith "todo"
            | RecordInfo fields ->
                let fieldsText =
                    fields
                    |> List.map (fun (name, t) -> $"{name.AsText 0}: {t.AsText 0}")
                    |> String.concat "; "
                $"{{ {fieldsText} }}"
            | Let (info, expr) ->
                match expr with
                | Sequence items ->
                    // $"let {info.Name} =\n{expr.AsText (depth + 1)}"
                    $"let {info.Name} =\n{expr.AsText 1}"
                | _ ->
                    $"let {info.Name} = {expr.AsText 0}"

            | NonCurriedLambda (args, expr) ->
                let argsText =
                    args
                    |> List.map (fun arg ->
                        match arg.TypeConstraint with
                        | Some t -> $"({arg.Name} : {t})"
                        | None -> arg.Name
                    )
                    |> String.concat " "
                $"fun {argsText} ->\n{expr.AsText 1}"
            |> fun s ->
                let ws = " " * (depth + depth + depth + depth)
                ws + s
                // s.Replace ("{WS}", "    ")
            // | _ ->
            //     ""
    let parseCallArgs callee (args: Token list) =
        // let f arg =
        //     match arg with
        //     | Line
        args |> List.map parseExpression
    module Data =
        let takeToken token =
            match token with
            | Token.Line (start::tail) ->
                match start with
                | Token.Line rest ->
                    let next, more = takeToken start
                    // next, Token.Line (tail @ [ more ])
                    next, Token.Line (more :: tail)
                | _ ->
                    match tail with
                    | [ Token.Line rest ] ->
                        start, Token.Line rest
                    | [ token ] ->
                        start, token
                    | _ ->
                        start, Token.Line tail
            | _ -> token, Token.Null
            // match token with
            // | Token.Line (start::tail) ->
            //     match start with
            //     | Token.Line rest ->
            //         let next, more = takeToken start
            //         // next, Token.Line (tail @ [ more ])
            //         next, (more @ tail) // Token.Line (more :: tail)
            //     | _ ->
            //         match tail with
            //         | [ Token.Line rest ] ->
            //             start, rest //Token.Line rest
            //         | [ token ] ->
            //             start, [] // token
            //         | _ ->
            //             start, tail // Token.Line tail
            // | _ -> token, [] // Token.Null

        let takeWhileNot t token =
            let rec loop acc token =
                match takeToken token with
                | value, rest when value = t ->
                    // acc, [ token ]
                    acc @ [ token ], rest
                | value, Token.Null when value <> t ->
                // | value, [] when value <> t ->
                    failwith "ope"
                | token, rest ->
                    // let a, b = loop (acc @ [ token ]) (List.head rest)
                    // a, b @ List.tail rest
                    loop (acc @ [ token ]) rest
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
            let rest = rest |> Data.skipToken
            // let rest = rest |> List.skip 1
            // let range, rest = rest |> Data.takeWhileNot (Identifier "do")
            let range, rest = rest |> Data.takeWhileNot (Identifier "do")
            let rest = rest |> Data.skipToken
            ForLoop (bindings |> List.map parseExpression, parseExpression (Token.List range), parseExpression rest)
        | Token.List (Token.Identifier "type" :: Token.Identifier name :: rest)
        | Token.Line (Token.Identifier "type" :: Token.Identifier name :: rest) ->
            let typeArgs, rest = rest |> Token.Line |> Data.takeWhileNot (Identifier "=")
            let rest = rest |> Data.skipToken
            match rest with
            | Token.Sequence fieldTokens ->
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
                |> Data.skipToken
                |> parseExpression
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
        | Token.List (callee::args)
        | Token.Line (callee::args) ->
            let binaryOps = [ "+-/*^"; ".." ]
            match args with
            | Identifier op :: otherArgs
                    when
                        (op.Length = 1 && binaryOps[0].Contains op) ||
                        (List.tail binaryOps |> List.exists (fun s -> s.Contains op)) ->
                // todo: do the conversion on all args
                Call (parseExpression (Identifier op), List.map parseExpression (callee::otherArgs))
            | Identifier "<-" :: rest ->
                let value = parseExpression (Token.Line rest)
                Assign (parseExpression callee, value)
            | _ ->
                let callee = parseExpression callee
                let args = parseCallArgs callee args
                match callee with
                | Ignore -> Ignore
                | _ -> Call (callee, args)
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
                // Let ({ Name = name; Type = None }, parseLetValue value)
                Let (
                    { Name = name; TypeConstraint = None },
                    parseExpression (Token.List value)
                )
            | _ ->
                // todo: parse each arg
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
                        parseLetValue [ Token.Line value ]
                Let (
                    { Name = name; TypeConstraint = None },
                    NonCurriedLambda (args, letValue)
                )

        | _ ->
            Throw (Token.List tokens)

match run (manyTill line eof) normalCode with
// match run (manyTill line eof) normalCode with
| Success(o, unit, position) ->
    let filteredExpressions =
        o
        |> List.filter (fun (_, items) -> items.Length <> 0)
        |> List.map (fun (ws, items) ->
            ws, items |> List.filter (function Comment _ -> false | _ -> true)
        )
    // let asdf =
        // getSubExpressions 0 filteredExpressions
    let asdf2 =
        tree 0 filteredExpressions
    let exprList = asdf2 |> List.map _.Flatten
    printfn "%A" exprList
    // match exprList[0] with
    let result =
        exprList
        |> List.map (fun expr ->
            // match expr with
            // | Token.List exprs ->
            //     Parse.parseExpression (Token.Line exprs)
            //     |> printfn "%A"
            // | Token.Line exprs ->
            //     // Parse.parseLet exprs
            //     Parse.parseExpression (Token.Line exprs)
            //     |> printfn "%A"
            // | _ -> ()
            expr
            |> Parse.parseExpression
            |> fun expr ->
                printfn "%A" expr
                expr
        )
    let output =
        result
        |> List.map (fun expr -> expr.AsText 0)
        |> List.iter (printfn "%s")
    // let result1 = treeify o
    // let result = collectExpressions2 0 o
    // let asdf = collectExpressions exprs
    ()
| Failure(s, parserError, unit) ->
    ()
