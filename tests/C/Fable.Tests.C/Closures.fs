module Fable.Tests.C.Closures
let test () =
    let mutable count = 0
    let count2 = count + 2
    let closure () =
        count <- count + 1
        count + 1

    for i in 1..10 do
        printfn $"{closure ()}"
let myEffect() =
    printfn "Effect!"
    fun () -> printfn "Cleaning up"

// Method from a Python module, expects a function
// that returns another function for disposing
let useEffect (effect: unit -> (unit -> unit)): unit =
    let fn = effect ()
    fn ()

// Fails, Fable thinks this is a 2-arity function
let effects () =
    useEffect myEffect
