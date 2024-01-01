namespace Fable.C

// open System.Text
// open Fable.C.AST
open System
open System.Collections

type SourceBuilder(?ident: int, ?s: string) =
    let ident = defaultArg ident 0
    let sb = System.Text.StringBuilder(defaultArg s "")
    
    let (*) (c: char) (i: int) =
        let sb = System.Text.StringBuilder()
        for _ in 1..i do
            sb.Append(c) |> ignore
        sb.ToString()
        
    let leading_whitespace = (char ' ') * ident
    
    static member From(s: string) = SourceBuilder(0, s)
    
    member this.Append(s: string) =
        sb.Append(s) |> ignore
        this
        
    member this.AppendText(s: string) =
        let lines = s.Split(char "\n") |> Array.filter (not << System.String.IsNullOrEmpty)
        for line in lines do
            sb.AppendLine(leading_whitespace + line)
            |> ignore
//        sb.Append(leading_whitespace + s.Replace("\n", "\n" + leading_whitespace))
//        |> ignore
        this
    member this.AppendLine(s: string) =
        sb.AppendLine(leading_whitespace + s)
        |> ignore
        this
        
    member this.BeginBlock f =
        let b = SourceBuilder(ident + 4)
        f b
        |> ignore
        sb.Append(b.ToString())
        |> ignore
        this

    member this.AppendBlock (s: string) =
        let b = SourceBuilder(ident + 4)
        b.AppendText(s)
        |> sb.Append
        |> ignore
        
    member this.AppendBuilder(b: SourceBuilder) =
        this.AppendText(b.ToString())
        
    member this.AppendBuilderInline(b: SourceBuilder) =
        this.Append(b.ToString())
        
    override this.ToString() = sb.ToString()
    
type CompiledOutputBuilder() =
    let history = Generic.List<{| data: string; source: string |}>()
    let callee (s: string) = 
        try
            let line = s.Split('\n').[2]
            let items = line.Split(" in " |> Array.ofSeq)
            let (file, line) =
                let line_and_file = (items.[1].Split('/') |> Array.last).Split(':')
                line_and_file.[0], line_and_file.[1]
            let method = items.[0].Split("at " |> Array.ofSeq).[1]
            $"{file} {line}: {method}"
        with ex ->
            // log $"{ex}"
            s
    member _.History = history
    member this.AppendLine(s: string) =
        // history.Add({| data = $"{s} /* {callee Environment.StackTrace} */\n"; source = callee Environment.StackTrace |})
        #if !FABLE_COMPILER
        history.Add({| data = $"{s}\n"; source = callee Environment.StackTrace |})
        #else
        history.Add({| data = s; source = callee "" |})
        #endif
        this
    member this.Append(s: string) =
        #if !FABLE_COMPILER
        history.Add({| data = s; source = callee Environment.StackTrace |})
        #else
        history.Add({| data = s; source = callee "" |})
        #endif
        this
    override _.ToString() =
        String.Join("\n", history |> Seq.map (fun item -> item.data)) // + $" /* {item.source} */"))
