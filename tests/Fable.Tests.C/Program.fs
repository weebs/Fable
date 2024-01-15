// For more information see https://aka.ms/fsharp-console-apps

open System.Collections.Generic
// todo: doesn't work, value isn't initialized
// todo: how to handle ref count on static objs?
// let staticDictionary = Dictionary<int, string>()
let collections_test () =
    let i = System.Collections_Generic.List<int>()
    let i2 = System.Collections_Generic.Dictionary<int, string>()
    let i3 = System.Collections_Generic.Dictionary<string, string>()
    let d = Dictionary<int, string>()
    // todo: this will only work with interning or if GetHashCode calls the right method
    let d2 = Dictionary<string, string>()
    let l = List<int>()
    l.Add(420)
    d.Add(1, "1")
    printfn "Hello from F#"
    printfn $"{d[0]}"
    printfn $"{l[0]}"

let constValue = 0
let lambda_test () =
    let rec f value =
        if value = 0 then f (value + 1)
        else value + constValue + 1
    printfn $"{f 43}"

// type DU<'t> =
type DU =
    | A of foo: string
    | B of int
    // | C of 't

let duTest () =
    let du = DU.A "420"
    let du2 = DU.B 69
    // let du3 = DU.C 420.69
    printfn $"{du} {du2}"
