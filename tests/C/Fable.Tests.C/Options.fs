module Fable.Tests.C.Options

open System.Collections.Generic

let test () =
    let a = Some 420
    let b = Some ""
    let c = Some (List<int>())

    let e: int option = None
    ()

let valueTest () =
    let a = ValueSome 420
    let b = ValueSome ""
    let c = ValueSome (List<int>())
    let e = ValueNone
    ()
