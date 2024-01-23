module Ast


open System
open FParsec
open FSharp.Data.LiteralProviders
open System.Runtime.InteropServices


module libc =
    [<DllImport("libc")>] 
    extern void printf(string s)
// let inline printfn (format: Printf.TextWriterFormat<_>) =


// System.Console.SetOut(new System.IO.StringWriter())

// let inline printfn (text: string) =
//     libc.printf (text + "\n")



// let file_contents () = TextFile.``gtk.txt``.Text

// let file_contents () = TextFile.``emscripten.txt``.Text

let debug p text =
    let result = run p text
    printfn $"%A{result}"
    result

// Console.WriteLine (file_contents.Split('\n').Length)

type FileElement = { Indentation: int; Prefix: string; StartToken: string; Type: string; Text: string }

module File =
    let parser = 
        (pstring "|" <|> pstring "`" <|> pstring " ") >>. 
        (attempt (manyChars (pchar ' ' <|> pchar '|' <|> pchar '`'))) .>>. 
        (pstring "`-" <|> pstring "|-" <|> pstring "-" <|> pstring "|") .>>.
        (attempt (manyCharsTill anyChar (pchar ' ')) <|> manyChars anyChar) .>>.
        manyChars anyChar
        |>> (fun (((indentation, startToken), title), text) -> { 
            Prefix = indentation
            Indentation = indentation.Length
            StartToken = startToken
            Type = title
            Text = text
        })
    let start =
        pstring "TranslationUnitDecl" >>. manyChars anyChar |>> (fun state -> "TranslationUnitDecl" + state)
        

    let parse (file_contents: string) =
        // let file_contents () = TextFile.GL.``gl.h``.Text
        // let lines () = (file_contents ()).Split '\n'
        let lines = file_contents.Split '\n'
        // printfn $"{file_contents}"
        printfn $"File size: {lines.Length} newlines"


        let mutable running = false
        let parsedHeader = run start (lines.[0])
        // let lines = lines ()

        async {

            return!
                lines |> Seq.skip 1
                |> Seq.take (lines.Length - 2) // todo: why - 2?
                |> Seq.chunkBySize ((lines.Length - 2) / 5)
                |> Seq.map (fun lines -> async { return lines |> Seq.map (fun line -> line, run parser line) })
                |> Seq.toArray
                |> Async.Parallel
        }

// printfn $"{parsedContent}" // |> Array.iter (printfn "%A")

module Parser =
    // let cType =
    //     manyCharsTill anyChar (pchar ' ')
    let functionArguments : Parser<string array, unit> =
        // manyStrings (manyCharsTill anyChar (pchar ','))
        manyChars anyChar |>> (fun s -> s.Split ", ")
    let returnType =
        manyCharsTill anyChar (pchar '(')
    let fn2 = 
        (pipe2 returnType functionArguments (fun returns args -> (returns, args)))
    let functionSignature =
        returnType .>>. 
        // ((manyCharsTill anyChar (pchar ')') |>> run functionArguments))
        ((manyCharsTill anyChar (pchar ')') >>= (fun state -> fun stream -> Reply(state.Split ", "))))
        |>> (fun (typ, args) -> {| Return = typ; Args = args |})
    let functionDeclarationText =
        (optional (pstring "invalid "))
        >>. (optional (pstring "used "))
        >>. manyCharsTill anyChar (pchar ' ') .>>.
        (attempt (pstring "used " |>> Some) <|>% None) .>>
        // manyChars (noneOf ['\'']) '\''
        (pchar '\'') .>>. 
        manyCharsTill anyChar (pchar '\'') .>>.
        (attempt (manyChars anyChar))
        // |>> fun ((name, signature), rest) -> {| Name = name; Signature = run functionSignature signature; Remaining = rest |}
        |>> fun ((name, signature), rest) -> {| Name = name; Signature = run functionSignature signature; Remaining = rest |}
    let nodeAddress =
        manyCharsTill anyChar (pchar ' ') .>> (optional ((pstring "parent " <|> pstring "prev ") .>> manyCharsTill anyChar (pchar ' ')))
    let fileSource =
        // attempt (pstring "<<invalid sloc>> ") <|> 
        // (pchar '<' >>. ((attempt (pstring "<invalid sloc>>")) <|> 
        (pstring "<<invalid sloc>> ") <|> (manyCharsTill anyChar (pstring "> " |>> (fun _ -> '>')))

        // |>> fun (_, file) -> file
    let fileSource2 =
        ((pstring "<invalid sloc> ") <|> (manyCharsTill anyChar (pchar ' ' |>> ignore <|> eof)))
        .>> optional (pstring "referenced ")
        .>> optional (pstring "implicit ")
        .>> optional (pstring "invalid ")
    let optionalKeyword : Parser<unit,unit> =
        optional (pstring "referenced ")
        .>> optional (pstring "implicit ")
        .>> optional (pstring "invalid ")
    let functionDecl =
        nodeAddress 
        .>>. fileSource
        .>> optionalKeyword
        // pchar '<' .>>. manyCharsTill anyChar (pstring "> " |>> (fun _ -> '>'))
        |>> (fun (addr, source) -> {| Address = addr; Source = source |})
        .>>. manyCharsTill anyChar (pchar ' ')
        |>> (fun (r, src) -> {| r with Source2 = src |})
        .>>. manyChars anyChar
        |>> (fun (r, src) -> {| r with Name = src.Split(" ")[0]; Text = src; Declaration = run functionDeclarationText src |})

    let typedefDecl =
        nodeAddress .>>. fileSource .>>. fileSource2
        .>>. optional ((pstring "referenced ") <|> pstring "implicit ")
        .>>. manyCharsTill anyChar (pchar ' ')
        .>> pchar '\''
        .>>. manyCharsTill anyChar (pchar '\'')
        .>>. (attempt (
                pchar ':'
                >>. pchar '\''
                >>. manyCharsTill anyChar (pchar '\'')
            ) <|>% "")
    let parmVarDecl =
        nodeAddress
        .>>. fileSource
        .>>. fileSource2
        // .>>. many1 (many1Chars (noneOf [ ' ' ]) .>> (pchar ' '))
        .>>. (manyCharsTill (noneOf [ '\'' ]) (pchar '\'')) //.>>. sepBy1 (many1Chars (noneOf [ ' ' ])) (pchar ' '))
        .>>. (manyCharsTill anyChar (pchar '\''))
        .>>. (attempt (pchar ':' >>. pchar '\'' >>. manyCharsTill anyChar (pchar '\'')) <|>% "")
        .>> eof

    
    module Type =
        type ArgType =
            | TypeName of string
            | FunctionPrototype of returnType: ArgType * args: (ArgType list)
        let parse, parseRef = createParserForwardedToRef<ArgType, unit>()
        let parseArgs, parseArgsRef = createParserForwardedToRef<ArgType, unit>()
        // For types like (struct Foo *, int, unsigned long, char**)
        let simpleType =
            attempt (manyCharsTill anyChar (pchar ' ' <|> pchar '*') .>>. manyChars (pchar '*') |>> (fun (a, b) -> a + b)) <|> manyChars anyChar
            
        // do parseRef := 
        let runParser (state: string) : ParserResult<ArgType, _> =
            if state.Contains "(" then // Function pointer
                // FunctionPointer (TypeName "", [])
                run parse state
            else
                run (simpleType |>> TypeName) state
        let functionPrototypeName =
            manyCharsTill anyChar (pchar ')') .>> pchar '('
        let parseFn =
            manyCharsTill anyChar (pchar '(') .>> pchar '*' .>> functionPrototypeName .>>. (parse) .>> pchar ')'
        let tuple : Parser<_,unit> = 
            many1CharsTill (noneOf [','; ')']) (pchar '(') .>> pchar '*' .>>. manyCharsTill anyChar (pchar ')') .>> pchar '(' .>>. many parse .>> (optional (pchar ')'))  //manyCharsTill anyChar (pchar ')')
        do parseRef.contents <-
                // attempt simpleType |>> TypeName
                // manyChars (noneOf [ '('; ' '; ')' ]) |>> (fun name -> printfn "\nmanyChars noneOf [ (; _; ) ]\n"; TypeName "oi")
                // manyCharsTill anyChar
                // (attempt tuple |>> fun r -> printfn $"%A{r}"; Unchecked.defaultof<_>) <|>
                // (attempt (
                //     parseFn
                //     // ((manyCharsTill anyChar (pchar '(' >>. pchar '*' >>. manyCharsTill anyChar (pchar ')' >>. pchar '(')) .>>. (many parse)) 
                //         |>> fun (returnType, args) -> 
                //                 FunctionPrototype (TypeName <| "void (*) " + returnType, [args])
                //     )
                // )
                (attempt (tuple |>> fun ((returnType, name), argType) -> FunctionPrototype (TypeName returnType, argType)))
                <|> (attempt (many1CharsTill anyChar (pchar ',') .>> pchar ' ' |>> TypeName))
                <|> (attempt (many1CharsTill anyChar (pchar ')') |>> TypeName))
                <|> (many1Chars (noneOf [','; ')'; '(']) |>> TypeName) // (attempt (many1Chars (noneOf [ '('; ','; ')' ]) .>> pchar '\n' |>> TypeName))

    let fieldDecl =
        nodeAddress .>>.
        fileSource
        .>>. fileSource2 |>> fun ((addr, src), src2) -> {| Address = addr; Source = [| src; src2 |] |}
        .>>. (manyCharsTill anyChar (pchar ' ')) |>> fun (r, name) -> {| r with Name = name |}
        .>> pchar '\'' .>>. manyCharsTill anyChar (pchar '\'') |>> fun (r, typ) -> {| r with Type = typ |}
        .>>. manyChars anyChar |>> fun (r, rest) -> {| r with Rest = rest |}

    let address =
        nodeAddress .>>. 
        fileSource .>>. 
        fileSource2 //.>>.
    let record =
        nodeAddress .>>. 
        fileSource .>>. 
        fileSource2 //.>>.
        |>> (fun ((addr, fileSource), fileSource2) -> {| Address = addr; Source = [| fileSource; fileSource2 |] |}) .>>
        (pstring "struct" <|> pstring "union") .>>
        (optional (pchar ' ')) .>>. 
        (attempt (manyCharsTill anyChar (pchar ' ')) <|> manyChars anyChar) .>>. 
        (attempt ((attempt (manyChars anyChar) <|>% "") <|> preturn ""))
        |>> fun ((r, structName), rest) -> {| r with Name = structName; Rest = rest |}
    let enumDecl =
        nodeAddress .>>. fileSource .>>. fileSource2
        .>>. (attempt (manyChars anyChar) <|>% "")
    // let enumConstantDecl =
    //     address .>>. manyCharsTill anyChar (pchar ' ') .>> pchar '\'' .>>. manyCharsTill anyChar (pchar '\'')

    let varDecl =
        address .>>. manyCharsTill anyChar (pchar ' ') .>> pchar '\'' .>> manyChars (noneOf [ '\'' ])  .>> pchar '\''

    let enumConstantDecl =
        address .>>. manyCharsTill anyChar (pchar ' ') .>> pchar '\'' 
        .>>. manyChars (noneOf [ '\'' ]) |>> fun (rest, t) -> rest, run Type.parse t
        .>> pchar '\''

    let parsers = Map.ofList [
        "FunctionDecl", functionDecl |>> sprintf "%A"
        "RecordDecl", record |>> sprintf "%A"
        "TypedefDecl", typedefDecl |>> sprintf "%A"
        // "ParmVarDecl", parmVarDecl |>> sprintf "%A"
        "EnumDecl", enumDecl |>> sprintf "%A"
        "EnumConstantDecl", enumConstantDecl |>> sprintf "%A"
    ]
type EnumCase = Label of string * Parser.Type.ArgType * value: obj
type Decl = Var of string * Parser.Type.ArgType | Enum of string * Parser.Type.ArgType * EnumCase[] | Struct of string * (string * Parser.Type.ArgType)[] | Typedef of string * Parser.Type.ArgType | Function of string * returns: Parser.Type.ArgType * args: (string * Parser.Type.ArgType)[]
let parseFile (parsed: (string * FileElement)[]) : Async<Decl[][]> =
    let byIndex =
        parsed
        |> Array.mapi (fun i item -> i, item)
        // |> Map.ofArray
    let declsByIndex =
        parsed
        |> Array.filter (fun (_, item) -> item.Indentation = 0)
        |> Array.mapi (fun i item -> i, item)
        |> Map.ofArray
    let byType = 
        parsed |> Array.mapi (fun i item -> i, item) 
        |> Array.groupBy (fun (index, (line, parsed)) -> parsed.Type)
        |> Map.ofArray
    let leafNodes index = 
        let (_, element) = parsed |> Seq.skip index |> Seq.head
        parsed |> Seq.skip (index + 1) |> Seq.takeWhile (fun (string, el) -> el.Indentation > element.Indentation)
    let leafNodesWithIndex index = 
        let (_, element) = parsed |> Seq.skip index |> Seq.head
        parsed |> Seq.mapi (fun i elem -> i, elem) |> Seq.skip (index + 1) |> Seq.takeWhile (fun (string, (_, el)) -> el.Indentation > element.Indentation)
    let parseTypedef element =
        match run Parser.typedefDecl element.Text with
        | Success ((((result, name), typ), emptyMaybe), state, pos) ->
            match run Parser.Type.parse typ with
            | Success (parsedType, _, _) ->
                Result.Ok <| Typedef (name, parsedType)
            | error -> Result.Error $"TypedefDecl:\n{error}"
            // printfn $"Parsing: {typ}"
            // |> ignore
        | error -> 
            Result.Error $"Typedef error %A{error}"
    [|
        for kv in byType do 
            yield (async { 
                return [|
                let typ = kv.Key
                let items = kv.Value
                printfn $"typ = {typ}"
                if typ = "RecordDecl" then
                    printfn $"Record declarations! {items.Length}"
                    // let index = fst items[0]
                    // let (string, element) = snd items[0]
                    // let allTypes = [|
                    for (index, (string, element)) in items do
                        // printfn $"{index} {element.Indentation}: {element.Text}"
                        match run Parser.record element.Text with
                        | Success (result, _, _) ->
                        let elements = parsed |> Seq.skip (index + 1) |> Seq.takeWhile (fun (string, el) -> el.Indentation > element.Indentation)
                        // let elements = parsed |> Seq.skip index + 1 |> Seq.take 2
                        for (typ, nodes) in (elements |> Seq.groupBy (fun (text, e) -> e.Type)) do

                            if typ = "FieldDecl" then
                                let fields = [|
                                    for (text, element) in nodes do
                                        match run Parser.fieldDecl element.Text with
                                        | Success (result, state, pos) -> 
                                            // yield (sprintf "%A" result)
                                            // yield (sprintf "%A" (run Parser.Type.parse result.Type))
                                            match run Parser.Type.parse result.Type with
                                            | Success (t, state, pos) -> 
                                                // printfn $"    {result.Name} = {t}"
                                                yield (result.Name, t)
                                            | error -> printfn $"%A{error}"
                                            // printfn $"\t\t\t\t {result.Name}:\n{run Parser.Type.parse result.Type}"
                                            // let s = "\n"
                                            ()
                                        | result -> 
                                            printfn $"%A{result}"
                                            ()
                                |]
                                if result.Name = "definition" then
                                    let (_, (_, typedefName)) =
                                        byIndex
                                        |> Array.find (fun (i, element) ->
                                            i > index && (snd element).Indentation = 0)
                                    let typedef = parseTypedef typedefName
                                    match typedef with
                                    | Result.Ok (Typedef (name, argType)) ->
                                        yield Struct (name, fields)
                                    | _ ->
                                        failwith $"%A{typedefName} %A{fields}"
                                else
                                    yield Struct (result.Name, fields)
                        | error -> printfn $"{error}"
                                // yield typ
                                
                            // for (text, element) in elements do
                                // printfn "%A" element
                                // printfn "%A" text
                    // |] 
                    // |> Array.distinct
                    // printfn $"%A{(Array.distinct allTypes)}"
                    ()
                elif typ = "TypedefDecl" then
                    for (index, (str, element)) in items do
                        match parseTypedef element with
                        | Result.Ok def -> yield def
                        | Result.Error msg -> printfn "%s" msg
                elif typ = "FunctionDecl" then
                    // printFunctionDecl items typ

                    for (index, (str, element)) in items do
                        // printfn $"%A{element}"
                        match (run Parser.functionDecl element.Text) with
                        | Success (result, state, pos) ->
                            // printfn $"finding values for %A{element}"

                            let paramValues = 
                                parsed |> Seq.skip (index + 1) |> Seq.takeWhile (fun (_, el) -> el.Indentation > element.Indentation)
                                |> Seq.filter (fun (_, element) -> element.Type = "ParmVarDecl")
                            let args = [|
                                for (paramLine, paramElement) in paramValues do
                                    // printfn $"{paramLine}"
                                    match (run Parser.parmVarDecl paramElement.Text) with
                                    | Success ((((result, fn_name), fn_or_typedef), fn), state, pos) ->
                                    // | Success (((parmVarDecl, firstChar), typ), state, pos) ->
                                        // printfn $"{paramElement.Text}"

                                        // let typ =
                                            if fn.Contains ("(*)".ToUpper()) then
                                                // printf $"{fn_or_typedef}\t\t"
                                                // printfn $"{fn}"
                                                match (run Parser.Type.parse fn) with
                                                | Success (result, state, pos) -> 
                                                    // Result.Ok result
                                                    if not (String.IsNullOrEmpty fn_or_typedef) then
                                                        yield (fn_name, Parser.Type.ArgType.TypeName fn_or_typedef)
                                                    else
                                                        yield (fn_name, result)
                                                | Failure (text, error, state) -> 
                                                    // Result.Error <| sprintf "%A" error
                                                    ()
                                            else
                                                // printfn <| sprintf "%A" (Result.Ok (Parser.Type.TypeName fn_or_typedef))
                                                yield (fn_name, Parser.Type.TypeName fn_or_typedef)
                                                // printfn $"%A{result} {fn_or_typedef} (fn: ){fn}"

                                        // printfn $"%A{typ}"
                                        // printfn $"%A{parmVarDecl}"
                                        // printfn $"{firstChar[0]}{typ}"
                                    | result -> 
                                        printfn $"%A{result}"
                            |]
                            match result.Declaration with
                            | Success (decl, _, _) ->
                                match decl.Signature with
                                | Success (signature, _, _) ->
                                    yield Function ((snd decl.Name |> Option.defaultValue "") +  (fst decl.Name), Parser.Type.TypeName signature.Return, args)
                                | _ -> ()
                            | _ -> ()
                            // match (run  parmVarDecl result)
                        | parsed -> 
                            printfn $"%A{parsed}"
                    ()
                elif typ = "EnumDecl" then
                    for (index, (str, element)) in items do
                        let items = leafNodesWithIndex index |> Seq.toArray
                        let (_, enumTypeDef) = parsed |> Seq.skip (index + items.Length + 1) |> Seq.head
                        // printfn $"{element.Text}"
                        // printfn "Item after enum"
                        // printfn $"%A{enumTypeDef}"
                        let enumCases = [|
                            for (i, (str, el)) in items do
                                match el.Type with
                                | "EnumConstantDecl" ->
                                    match run Parser.enumConstantDecl el.Text with
                                    | Success (((addr, caseName), (Success (t, _, _))), _, _) ->
                                        let nodes =
                                            leafNodesWithIndex i
                                            |> Seq.filter (fun (_, (_, node)) ->
                                                node.Type <> "FullComment" &&
                                                node.Type <> "ParagraphComment" &&
                                                node.Type <> "TextComment")
                                        if Seq.isEmpty nodes then
                                            Label (caseName, t, null)
                                        else
                                            let (constIndex, constExpr) = leafNodesWithIndex i |> Seq.head
                                            let _nodes = leafNodes constIndex |> Array.ofSeq 
                                            let (_, valueExpr) = _nodes |> Seq.find (fun (nodeText, node) -> node.Type = "value:")
                                            let (wasParsed, value) = valueExpr.Text.Split(" ") |> Array.last |> Int32.TryParse
                                            Label (caseName, t, value)
                                    | _ -> ()
                                | _ -> ()
                        |]
                        // printfn $"{str}"
                        // let nodes = leafNodes index
                        // let enumCases = [|
                        //     for (str, e) in nodes do
                        //         match e.Type with 
                        //         | "EnumConstantDecl" ->
                        //             match run Parser.enumConstantDecl e.Text with
                        //             | Success (((addr, caseName), (Success (t, _, _))), _, _) -> 
                        //                 Label (caseName, t, null)
                        //             | _ -> ()
                        //         | _ -> ()
                        //         //     "                           Error: " + str
                        // |]
                        // for str in enumCases do
                        //     printfn $"{str}"
                        if enumTypeDef.Type = "TypedefDecl" then
                            let enumDef = run Parser.enumDecl element.Text
                            let enumTypedef = run Parser.typedefDecl enumTypeDef.Text
                            match enumTypedef with
                            | Success (((addr), name), _, _) ->
                                yield Enum (name, Parser.Type.ArgType.TypeName "", enumCases) // |> Array.map (sprintf "%A"))
                            | _ -> ()
                            // let otherItems = items |> Array.filter (fun (_, elem) -> not (Parser.parsers.ContainsKey elem.Type)) |> Array.map (sprintf "%A")
                            // let items = items |> Array.filter (fun (_, elem) -> Parser.parsers.ContainsKey elem.Type) |> Array.choose (fun (_, e) -> run Parser.parsers[e.Type] e.Text |> function Success (state, _, _) -> Some state | _ -> None)
                            // yield Enum (sprintf "%A" enumTypedef, Parser.Type.ArgType.TypeName "", Seq.toArray nodes) // Array.map (sprintf "%A") items)

                            // printfn "Enum!"
                            // printfn $"%A{otherItems}"
                            // printfn $"\n\n%A{enumDef}\n%A{enumTypeDef}\n\n%A{items}"
                        else
                            yield Enum (str.Split(" ") |> Array.last, Parser.Type.ArgType.TypeName "", enumCases) // |> Array.map (sprintf "%A"))
                            // yield Enum ("", Parser.Type.ArgType.TypeName "", items) // |> Array.map (sprintf "%A"))
                            // for i in 1..20 do printfn $"No type decl for {index} %A{element}"
                else
                    // if Parser.parsers.ContainsKey typ then
                    //     for (index, (str, element)) in items do
                    //         printfn $"| {element.Type} {element.Text}"
                    //         |> ignore
                    //         printfn "\n"
                    ()
            |] 
        }) |] |> Async.Parallel
        // printfn $"{items}\n"
open Parser.Type
type Type() =
    static let rec mapBaseType (t: string) : string =
        match t.Trim() with
        | "_Bool" -> "bool"
        | "void" -> "unit"
        | "unsigned int" -> "uint"
        | "unsigned long" -> "uint64"
        | "float" -> "single"
        | "unsigned char" -> "byte"
        | "signed char" -> "sbyte"
        | "short" -> "int16"
        | "unsigned short" -> "uint16"
        | "long" -> "int64"
        | "unsigned __int128" -> "unsigned__int128"
        | t when t.StartsWith "const " -> mapBaseType (t.Substring("const ".Length))
        | t when t.StartsWith "struct " -> mapBaseType (t.Substring("struct ".Length))
        | t when t.StartsWith "enum " -> mapBaseType (t.Substring("enum ".Length))
        | t when t.Contains "[" && t.EndsWith "]" ->
            match mapBaseType (t.Substring(0, t.IndexOf("[") - 1)) with
            | "unit" -> "nativeint"
            | t -> "nativeptr<" + t + ">" //, t.Length - (t.IndexOf("[")) - 1)) + ">"
        | t -> t
    static member toFSharp t : string = Type.toFSharp (false, t)
    static member toFSharp (isStruct: bool, t) : string =
        match t with
        | TypeName name -> 
            let parts = name.Split(' ')
            if name.Contains "*" then
                let n = (name.ToCharArray() |> Array.filter (fun c -> c = '*')).Length
                ([1..n] |> List.map (fun _ -> "nativeptr<") |> String.concat "")
                + (name.Split("*").[0].Trim() |> mapBaseType)
                + ([1..n] |> List.map (fun _ -> ">") |> String.concat "")
                |> function
                | "unsigned __int128" -> "unsigned__int128"
                | "nativeptr<char>" when not isStruct -> "string"
                // | "nativeptr<char>" when isStruct -> "string"
                | "nativeptr<unit>" -> "nativeint"
                | "nativeptr<nativeptr<unit>>" -> "nativeptr<nativeint>"
                | "nativeptr<nativeptr<GLvoid>>" -> "nativeptr<nativeint>"
                | "nativeptr<GLvoid>" -> "nativeint"
                | t -> t
            else
                mapBaseType name
        | FunctionPrototype (returns, args) when not isStruct -> 
            (args |> List.map Type.toFSharp |> String.concat " * ") + " -> " + (Type.toFSharp returns)
        | FunctionPrototype _ when isStruct ->
            "nativeint"
        | _else -> sprintf $"{__LINE__}: %A{_else}"
    static member toFSharpExtern t : string =
        match t with
        | TypeName name ->
            match name.Trim() with
            | "void" -> "void"
            | _ -> mapBaseType name
        | FunctionPrototype (returns, args) -> $"%A{returns} %A{args}"
    static member fsharpName (name: string) : string =
        let keywords = [ "type"; "base"; "end"; "class"; "struct"; "fun"; "params"; "when" ]
        match name with
        // | "type" -> "``type``"
        // | "base" -> "``base``"
        | name when List.contains name keywords -> $"``{name}``"
        | name -> name
