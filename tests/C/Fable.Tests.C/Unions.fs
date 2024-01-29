module Fable.Tests.C.Unions

type FooUnion =
    | A of string
    | B of int * double
    | C of System.Collections.Generic.List<int>

type BarUnion<'t> =
    | X of (int * int)
    | Y of string
    | Z of FooUnion
    | T of 't


let test () =
    let items = (System.Collections.Generic.List<int>())
    for i in 1..100 do items.Add i
    let meow = FooUnion.B (1, 42)
    let arf = BarUnion<int * int * int * int>.Z (FooUnion.C items)
    match arf with
    | T t -> printfn $"arf T = {t}"
    | X foo -> printfn $"arf X = {foo}"
    | Y s -> printfn $"arf Y = {s}"
    | Z fooUnion ->
        match fooUnion with
        | A str -> printfn $"fooUnion str = {str}"
        | B (i, f) -> printfn $"fooUnion Foo B ({i}, {f})"
        | C items ->
            // for item in items do
            for i in 0..items.Count - 1 do
                printfn $"fooUnion C items[{i}] = {items[i]}"
            match meow with
            | A str -> printfn $"str = {str}"
            | B (i, f) -> printfn $"Foo B ({i}, {f})"
            | C items ->
                // for item in items do
                for i in 0..items.Count - 1 do
                    printfn $"{items[i]}"
