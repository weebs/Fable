module Fable.Tests.C.Objects
type Foo = { bar: int }
type Bar(n: int) =
    member this.N = n

module Records =
    let test () =
        let f = { bar = 1234 }
        let bar = { f with bar = f.bar + 1 }
        printfn $"{f}"

    // let ref () = let foo = Fable.Ref<int>(1234) in ()
