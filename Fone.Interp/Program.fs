// For more information see https://aka.ms/fsharp-console-apps
open System.Xml.Schema
open FParsec
open FParsec.CharParsers
// let treeify (lines: (PrefixWhitespace * Token list) list) =
//     // let rec getChildren indent lines =
//     //     match lines with
//     //     | [] -> []
//     //     | (ws, value)::rest ->
//
//     let rec loop indent acc lines =
//         match lines with
//         | [] -> acc, []
//         | (ws, value: Token list)::rest ->
//             if ws.spaces > indent then
//                 let children, remaining = loop ws.spaces [] rest
//                 // acc @ (value :: children), remaining
//                 // acc @ (value :: children), remaining
//                 // acc @ [ Token.List [ yield! value; Token.List children ] ], remaining
//                 acc @ [ Token.List (value @ children)], remaining
//             elif ws.spaces < indent then
//                 acc, lines
//             else
//                 let children, remaining = loop ws.spaces [] rest
//                 let acc = acc @ [ yield! value; Token.List children ]
//                 loop indent acc remaining
//                 // value @ children, remaining
//                 // [], []
//                 // loop indent (acc @ value) rest
//                 // acc, lines
//     loop 0 [] lines
//     // let rec loop acc lines =
//         // match lines with
//         // | [] -> acc
//         // | _ ->
//         //     lines |> List.takeWhile (fun (indent, _) -> indent.)
// let rec collectExpressions2 (indent: int) (lines: (PrefixWhitespace * Token list) list) =
//     let rec getSubExpressions indent lines =
//         match lines with
//         | [] -> [], []
//         | (whitespace, tokens)::moreLines ->
//             if whitespace.spaces = indent then [], lines
//             elif whitespace.spaces < indent then [], lines
//             else
//                 let subExpressions, remainingLines = getSubExpressions whitespace.spaces moreLines
//                 let result = tokens @ [ Token.List subExpressions ]
//                 match remainingLines with
//                 | [] -> result, []
//                 | _ ->
//                     let asdf, asdf2 = getSubExpressions indent remainingLines
//                     result @ asdf, asdf2
//                 // tokens @ [ Token.List subExpressions ], remainingLines
//     /// Returns a list of sub expressions and then the remaining lines
//     let rec loop indent (acc: Token list) (lines: (PrefixWhitespace * Token list) list) : (_ list * _ list) =
//         match lines with
//         | [] -> acc, []
//         | asdf::moreLines ->
//             let (whitespace, tokens) = asdf
//             if whitespace.spaces = indent then
//                 // let subTokens, remainingLines = collectExpressions indent moreLines
//
//                 // todo
//                 // let subTokens, remainingLines = loop indent acc moreLines
//                 // let subTokens, remainingLines = loop indent [] moreLines
//
//                 // let acc = acc @ [ Token.List (tokens @ subTokens) ]
//                 // loop indent acc remainingLines
//                 // loop indent (acc @ [ yield! tokens; ])
//
//                 // todo
//                 // let foo = loop indent [ Token.List subTokens ] remainingLines
//                 // acc @ [ yield! tokens; Token.List subTokens ], remainingLines
//
//                 // Token.List tokens :: more, []
//                 // [], []
//
//                 let subExpressions, remainingLines = getSubExpressions indent moreLines
//                 let acc = acc @ [ Token.List (tokens @ subExpressions) ]
//                 loop indent acc remainingLines
//             elif whitespace.spaces > indent then
//                 // let subTokens, remainingLines = loop whitespace.spaces tokens moreLines
//                 // let result = acc @ [ Token.List (tokens @ subTokens) ]
//                 // let acc = tokens
//                 // (tokens @ subTokens), remainingLines
//                 let subTokens, remainingLines = loop whitespace.spaces tokens moreLines
//                 acc @ [ Token.List subTokens ], remainingLines
//                 // loop whitespace.spaces (acc @ tokens) moreLines
//                 // Token.List (tokens @ subTokens), moreLines
//             elif whitespace.spaces < indent then
//                 // let subExpressions = loop
//                 acc, lines
//                 // loop (acc @ [ Token.List tokens ])
//                 // [], []
//             else
//                 [], []
//     loop indent [] lines
// let rec collectExpressions (indent: int) (lines: (PrefixWhitespace * Token list) list) =
//     let rec loop (indent: int) acc lines : Token list * (PrefixWhitespace * Token list) list =
//         printfn "\n\n"
//         printfn $"Loop: Indent {indent}"
//         printfn $"%A{acc}"
//         // printfn $"%A{lines}"
//         match lines with
//         | [] -> acc, []
//         | (whitespace, tokens)::moreLines ->
//             printfn "%A" tokens
//             if whitespace.spaces = indent then
//                 printfn $"whitespace.spaces {whitespace.spaces} = indent {indent}"
//                 let rec innerLoop acc moreLines =
//                     match moreLines with
//                     | [] -> acc
//                     | _ ->
//                         let subExpressions, moreLines = loop whitespace.spaces [] moreLines
//                         innerLoop (acc @ subExpressions) moreLines
//                         // subExpressions, innerLoop
//                 let subExpressions = innerLoop [] moreLines
//                 // let subExpressions, moreLines = loop whitespace.spaces [] moreLines
//                 let result = (tokens @ subExpressions)
//                 // (acc @ [ Token.List result ]), moreLines
//                 (acc @ [ Token.List result ]), []
//                 // match moreLines with
//                 // | [] ->
//                 //     (acc @ [ Token.List result ]), moreLines
//                 // | _ ->
//                 //     // let more, moreLines2 = loop indent subExpressions moreLines
//                 //     let more = collectExpressions indent moreLines
//                 //     (acc @ [ Token.List result ] @ [ Token.List more ]), []
//                     // (acc @ [ result ] @)
//                 // let result = [ yield! tokens; Token.List subExpressions ]
//                 // (acc @ result), moreLines
//             elif whitespace.spaces < indent then
//                 printfn $"whitespace.spaces {whitespace.spaces} < indent {indent}"
//                 acc, lines
//             elif whitespace.spaces > indent then
//                 printfn $"whitespace.spaces {whitespace.spaces} > indent {indent}"
//                 // let subExpressions, moreLines = loop whitespace.spaces [] moreLines
//                 // let result = Token.List (tokens @ subExpressions)
//                 // let origResult = (acc @ [ result ]), moreLines
//                 // let moreResults, moreLines = loop indent [] moreLines
//                 let foo = collectExpressions indent moreLines
//                 let newResult = acc @ [ Token.List tokens ] @ [ Token.List foo ]
//                 newResult, []
//                 // let result = ()
//             else
//                 failwith "Shouldn't happen"
//         | _ -> Unchecked.defaultof<_>
//         |> fun result ->
//             result
//
//             // if whitespace.spaces > indent then
//             //     loop whitespace.spaces [] moreLines
//             // elif whitespace.spaces = indent then
//             //     (acc @ [ tokens ]), loop whitespace.spaces [] moreLines
//             // elif whitespace.spaces < indent then
//             //     acc,
//     let result, remainingLines = loop indent [] lines
//     match remainingLines with
//     | [] -> result
//     | _ -> result @ collectExpressions indent remainingLines
//
// let exprs =
//     List [
//         Ident "let"; Ident "foo"; Ident "x"; Ident "y"; List [
//             Ident "let"; Ident "n"; List [
//                 Ident "x"; Ident "+"; Ident "y"
//             ]
//             List [ Ident "n"; Ident "+"; Ident "x"; Ident "+"; Ident "y" ]
//             List [ Ident "foo"; List [ Ident "foo"; Ident "x"; List [ Ident "y" ] ] ]
//         ]
//     ]
let takes3 x y z : int = x + y + z
let print n =
    // printfn $"{n}"
    ()
do
    // print
    //     takes3 1 2 3 // error
    print
        (takes3 1 2 3) // fine
    let n =
        takes3
            1
            2 3 // fine
    let n2 =
        takes3 1
            2 3
    print n
    let f =
        takes3 1
          2
        3
    print f
type PrefixWhitespace = { tabs: int; spaces: int }
type Token =
    | Identifier of string
    | AnnotatedIdentifier of Token * Token
    | Whitespace of tabs: int * spaces: int
    | Number of string
    | List of Token list
    | Line of Token list
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
    (pchar ':' |>> string) <|>
    many1Chars (noneOf (List.map char [ "["; "]"; "("; ")"; ":"; " "; "\t"; "\n"; "\r"; ]))
    // many1 (noneOf (List.map char [ " "; "\t"; "\n"; "\r"; ]))
    .>> getWhitespace
    |>> fun id ->
        // Identifier (String.concat "" (List.map string id))
        Identifier id
// let annotatedParameter =
//     pchar '(' >>. ident .>> getWhitespace .>> pchar ':' .>> getWhitespace .>>. ident .>> getWhitespace .>> pchar ')' .>> getWhitespace
//     |>> fun id ->
//         AnnotatedIdentifier id
let number =
    numberLiteral NumberLiteralOptions.None ""
    |>> fun literal ->
        Number literal.String
let list =
    pchar '(' >>. many1 expr .>> pchar ')'
    |>> Token.List
let array =
    pchar '[' .>> getWhitespace >>.
    many1 (expr .>> (optional (pchar ';')) .>> getWhitespace) .>>
    getWhitespace .>> pchar ']' .>>
    getWhitespace
    |>> Array
    // pchar '(' >>. manyChars (noneOf ")") .>> pchar ')'
    // |>> Token.Identifier
// let expr =
//     ident
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
        // (attempt list) <|> ident
        emptyList <|> comment <|> array <|> list <|> number <|> ident
let line =
    // getWhitespace .>>. many expr .>> ((newline |>> ignore) <|> eof)
    getWhitespace .>>. (
        newlineOrEof
        <|> manyExprs
        // (attempt (many expr)) <|> (newline |>> fun _ -> [])
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
            Token.List (root @ flattened)


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
    { Name: string; Type: string option }
let parseArg (token: Token) =
    match token with
    | Token.List (Identifier name :: Identifier ":" :: [ Identifier typeName ]) ->
        { Name = name; Type = Some typeName }
    | Identifier name ->
        { Name = name; Type = None }
    | Token.List [] ->
        { Name = "unit"; Type = Some "unit" }
    // | _ -> { Name = ""; Type = None }
module rec Parse =
    type Expression =
        | Let of ArgInfo * Expression
        | Call of callee: Expression * args: Expression list
        | Ident of string
        | Throw of Token
        | Sequence of Expression list
        | Number of string
        | Ignore
        | ArrayLiteral of Expression list
        | Lambda of arg: ArgInfo * body: Expression
        | Assign of dest: Expression * value: Expression
        | NonCurriedLambda of args: ArgInfo list * body: Expression
    let parseCallArgs callee (args: Token list) =
        // let f arg =
        //     match arg with
        //     | Line
        args |> List.map parseExpression
    let parseExpression (token: Token) =
        match token with
        | Token.Line (Token.Identifier "let" :: rest)
        | Token.List (Token.Identifier "let" :: rest) ->
            parseLet (Token.Identifier "let" :: rest)

        | Token.List (Token.Identifier "fun" :: rest)
        | Token.Line (Token.Identifier "fun" :: rest) ->
            let args =
                rest
                |> List.takeWhile (fun t -> t <> Identifier "->")
                |> List.map parseArg
            let lambdaBody =
                rest
                |> List.skip (args.Length + 1)
                |> parseLetValue
            NonCurriedLambda (args, lambdaBody)
        | Token.List (callee::args)
        | Token.Line (callee::args) ->
            let binaryOps = "+-/*^"
            match args with
            | Identifier op :: otherArgs
                    when op.Length = 1 && binaryOps.Contains op ->
                // todo: do the conversion on all args
                Call (parseExpression (Identifier op), List.map parseExpression (callee::otherArgs))
            | Identifier "<-" :: rest ->
                let value = parseExpression (Token.Line rest)
                Assign (parseExpression callee, value)
            | _ ->
                let callee = parseExpression callee
                let args = parseCallArgs callee args
                Call (callee, args)
        | Identifier name -> Ident name
        | Line [] | Token.List [] ->
            Ignore
        | Token.Number s -> Number s
        | Comment s -> Ignore
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
                Let ({ Name = name; Type = None }, parseLetValue value)
            | Identifier "=" :: value ->
                Let ({ Name = name; Type = None }, parseLetValue value)
            | _ ->
                // todo: parse each arg
                let args =
                    rest
                    |> List.takeWhile (fun t -> t <> Identifier "=")
                    |> List.map parseArg
                let value =
                    rest
                    |> List.skip (args.Length + 1)
                    |> parseLetValue
                Let ({ Name = name; Type = None },
                    NonCurriedLambda (args, value)
                )

        | _ ->
            Throw (Token.List tokens)

match run (manyTill line eof) source2 with
| Success(o, unit, position) ->
    let asdf = getSubExpressions 0 o
    let asdf2 = tree 0 o
    let exprList = asdf2 |> List.map _.Flatten
    match exprList[1] with
    | Token.List exprs
    | Token.Line exprs ->
        Parse.parseLet exprs
        |> printfn "%A"
    | _ -> ()
    // let result1 = treeify o
    // let result = collectExpressions2 0 o
    // let asdf = collectExpressions exprs
    ()
| Failure(s, parserError, unit) ->
    ()
