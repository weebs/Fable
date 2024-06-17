module Fable.Tests.C.Byrefs

let modifiesByref (i: int byref) =
    i <- i * 2

let callsModifiesByrefTest () =
    let mutable n = 2
    modifiesByref &n
    printfn $"n = 4 ? (n = {n})"
let tests () =
    callsModifiesByrefTest ()

