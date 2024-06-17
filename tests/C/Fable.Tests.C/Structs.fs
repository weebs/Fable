module Fable.Tests.C.Structs

type [<Struct>] FooRecordStruct = {
    a: int
    b: float
}

type [<Struct>] FooStruct<'t>(item: 't) =
    member this.Item = item


let test () =
    let a = { a = 10; b = 420 }
    let b = { a with b = a.b * 2.0 }
    printfn $"{a}"

    printfn $"{b}"

    let c = FooStruct(1234)
    printfn $"{c.Item}"
