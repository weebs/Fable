module Fable.Tests.C.List

open System.Collections.Generic
let listCount () =
    let l = List<int>()
    for i in 0..100 do
        l.Add(i)
    for i in 0..l.Count - 1 do
        printfn $"l[{i}] = {l[i]}"
