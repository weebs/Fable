module Fable.Tests.C.Collections

let test () =
    let items = System.Collections.Generic.List<int>()
    for i in 1..10 do
        items.Add(i * 2)
    items.Add(11)
    printfn "hello"
    for i in 0..items.Count - 1 do
        printfn $"items[{i}] = {items[i]}"
