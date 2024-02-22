module Fone.Interp.TokenParser
open System.Xml.Schema
open FParsec
open FParsec.CharParsers
open Fable.C

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
// type Expr =
//     | Let of {| ident: string; value: Expr; body: Expr |}
//     | Call of {| callee: Expr; args: Expr list |}
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
        (emptyList <|> seq <|> comment <|> array <|> list <|> number <|> ident)
        .>> getWhitespace
let line =
    getWhitespace .>>. (
        newlineOrEof
        <|> manyExprs
    )
// forces parsers to be inferred
<@ run line "" @> |> ignore
