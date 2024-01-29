module Fable.Tests.C.Closures
let test () =
    let mutable count = 0
    let count2 = count + 2
    let closure () =
        count <- count + 1
        count + 1

    for i in 1..10 do
        printfn $"{closure ()}"
