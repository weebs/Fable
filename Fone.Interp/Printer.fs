module Fone.Interp.Printer

open AST.Unchecked
open Fable.C

let inline (*) (s: string) (n: int) =
    [| for i in 1..n do yield s |] |> String.concat ""
type Expression with
    member this.AsText (depth: int) : SourceBuilder =
        let sb = SourceBuilder()
        match this with
        | Range(from, until) ->
            sb
                .AppendBuilderInline(from.AsText 0)
                .Append(" .. ")
                .AppendBuilderInline(until.AsText 0)
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
                |> List.mapi (fun i sb -> if i < args.Length - 1 then sb.Append " " else sb)
                // |> String.concat (" " * 4)
                // |> List.map _.ToString()
                // |> String.concat " "
            // sb.Append $"{(callee.AsText 0).ToString()}"
            sb.Append "(" |> ignore
            sb.AppendBuilderInline(callee.AsText depth) // {argsText}"
                .Append " "
                |> ignore
            argsText |> List.map (sb.AppendBuilderInline) |> ignore
            sb.Append ")"
            // sb.Append
        | Ident s -> s |> sb.Append
        | Throw token -> failwith "todo"
        | Sequence expressions ->
            // expressions
            // // |> List.map (fun expr -> expr.AsText 0)
            // |> List.map (fun expr -> expr.AsText 0)
            // // |> String.concat "\n"
            // |> String.concat ("\n" + (" " * (depth + depth + depth + depth)))
            // |> sb.AppendLine
            sb.AppendLine "" |> ignore
            sb.BeginBlock(fun sb ->
                expressions
                |> List.map (fun expr -> expr.AsText 0)
                |> List.map sb.AppendBuilder
                |> ignore
            )
        | Number s -> s |> sb.Append
        | ForLoop(bindings, range, body) ->
            let bindingsText =
                bindings
                |> List.map (fun binding -> binding.AsText depth |> string)
                |> String.concat ", "
            sb
                .Append($"for {bindingsText} in {range.AsText 0 |> string} do")
                .BeginBlock(fun sb -> sb.AppendBuilderInline (body.AsText 0))
                // .ToString()
                // .BeginBlock()
            // $"for {bindingsText} in {range.AsText depth} do\n{body.AsText (depth + 1)}"
        | Ignore -> failwith "todo"
        | UnitConstant -> "()" |> sb.Append
        | ArrayLiteral expressions ->
            let expressionsText =
                expressions
                |> List.map (fun expr -> expr.AsText depth |> string)
                |> String.concat "; "
            $"[| {expressionsText} |]"
            |> sb.AppendLine
        | Lambda(arg, body) -> failwith "todo"
        | Assign(dest, value) ->
            $"{dest.AsText 0} <- {value.AsText 0}"
            |> sb.Append
        // | NonCurriedLambda(args, body) -> failwith "todo"
        | RecordInfo fields ->
            let fieldsText =
                fields
                |> List.map (fun (name, t) -> $"{name.AsText 0}: {t.AsText 0}")
                |> String.concat "; "
            $"struct {{ {fieldsText} }}"
            |> sb.Append
        | Let (info, expr) ->
            match expr with
            | Sequence items ->
                // $"let {info.Name} =\n{expr.AsText (depth + 1)}"
                // $"let {info.Name} =\n{expr.AsText 1}"
                // |> sb.Append
                sb.AppendLine($"let {info.Name} =")
                    .AppendBuilder(expr.AsText 1)
            | _ ->
                $"let {info.Name} = {expr.AsText 0}"
                |> sb.AppendLine
            // sb.Append($"let {info.Name} = ")
                // .AppendBuilder(expr.AsText 0)

        | NonCurriedLambda (args, expr) ->
            let argsText =
                args
                |> List.map (fun arg ->
                    match arg.TypeConstraint with
                    | Some t -> $"({arg.Name} : {t})"
                    | None -> arg.Name
                )
                |> String.concat " "
            // $"fun {argsText} ->\n{expr.AsText 1}"
            // sb.Append($"fun {argsText} ->\n")
                // .BeginBlock(fun sb -> sb.AppendBuilder (expr.AsText 1))
            // match expr with
            // | Sequence _ ->
            //     sb.Append($"fun {argsText} ->\n")
            //         .AppendBuilder (expr.AsText 0)
            // | _ ->
            sb.Append($"fun {argsText} -> ")
                .AppendBuilderInline (expr.AsText 0)
                // .BeginBlock(fun sb -> sb.AppendBuilder (expr.AsText 1))
            // |> sb.Append
        // |> fun s ->
        //     let ws = " " * (depth + depth + depth + depth)
        //     ws + s
            // s.Replace ("{WS}", "    ")
        // | _ ->
        //     ""
