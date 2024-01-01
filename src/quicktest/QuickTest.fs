module QuickTest

// Run `dotnet fsi build.fsx quicktest` and then add tests to this file,
// when you save they will be run automatically with latest changes in compiler.
// When everything works, move the tests to the appropriate file in tests/Main.
// Please don't add this file to your commits.

open System
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing

// let log (o: obj) = JS.console.log (o)
// // printfn "%A" o
//
// let equal expected actual =
//     let areEqual = expected = actual
//     printfn "%A = %A > %b" expected actual areEqual
//
//     if not areEqual then
//         failwithf "[ASSERT ERROR] Expected %A but got %A" expected actual
//
// let throwsError (expected: string) (f: unit -> 'a) : unit =
//     let success =
//         try
//             f () |> ignore
//             true
//         with e ->
//             if not <| String.IsNullOrEmpty(expected) then
//                 equal e.Message expected
//
//             false
//     // TODO better error messages
//     equal false success
//
// let testCase (msg: string) f : unit =
//     try
//         printfn "%s" msg
//         f ()
//     with ex ->
//         printfn "%s" ex.Message
//
//         if
//             ex.Message <> null
//             && ex.Message.StartsWith("[ASSERT ERROR]", StringComparison.Ordinal)
//                |> not
//         then
//             printfn "%s" (ex.StackTrace ??= "")
//
//     printfn ""
//
// let testCaseAsync msg f =
//     testCase
//         msg
//         (fun () ->
//             async {
//                 try
//                     do! f ()
//                 with ex ->
//                     printfn "%s" ex.Message
//
//                     if
//                         ex.Message <> null
//                         && ex.Message.StartsWith(
//                             "[ASSERT ERROR]",
//                             StringComparison.Ordinal
//                            )
//                            |> not
//                     then
//                         printfn "%s" (ex.StackTrace ??= "")
//             }
//             |> Async.StartImmediate
//         )
//
// let throwsAnyError (f: unit -> 'a) : unit =
//     let success =
//         try
//             f () |> ignore
//             true
//         with e ->
//             printfn "Got expected error: %s" e.Message
//             false
//
//     if success then
//         printfn "[ERROR EXPECTED]"
//
// let measureTime (f: unit -> unit) : unit =
//     emitJsStatement
//         ()
//         """
//    //js
//    const startTime = process.hrtime();
//    f();
//    const elapsed = process.hrtime(startTime);
//    console.log("Ms:", elapsed[0] * 1e3 + elapsed[1] / 1e6);
//    //!js
// """
type Vec<'t>() =
    let mutable items = [| Unchecked.defaultof<_> |]
    let mutable n = 0
    let mutable size = 1

    member this.Add(item) =
        if n >= size then
            let oldItems = items
            size <- size * 2
            items <- Array.zeroCreate (size * 2)

            for i in 0 .. n - 1 do
                items[i] <- oldItems[i]

        items[n] <- item
        n <- n + 1

type Count() =
    let mutable count = 0
    do printfn $"count = {count}"

    member this.Count =
        printfn "yo"
        count

    member this.Increment() = count <- count + 1
// let incrementCount (counter: Count) =
//     counter.Increment()
// let incrementCount2 (counter: Count) =
//     let count = counter
//     count.Increment()
let mutable n = 0

let returnsCounter () =
    let counter = Count()

    let toReturn =
        if n = 420 then
            let returns =
                if n = 400 then
                    let b = counter
                    printfn "yo"
                    b
                else
                    Count()

            if n = 10 then
                returns
            else
                Count()
        else
            let a = Count()
            a

    toReturn

let returnsCounter2 () = Count()

let returnsCount () =
    let count = Count()
    count.Increment()
    count.Count

let array () =
    let items =
        [|
            1
            2
            3
            4
        |]

    printfn $"items = {items}"
    // let otherItems = Array.create 10 ""
    let otherItems =
        [|
            ""
            ""
        |]

    otherItems

let genericsTest () =
    let vec = Vec<int>()

    for i in 1..5 do
        vec.Add i
// let tests () =
//     printfn "Running quick tests..."
//     let count = Count()
//     for i in 1..10 do
//         let counter = Count()
//         counter.Increment()
//         // todo: Auto-release is needed for this to work
//         incrementCount (Count())
//         incrementCount (returnsCounter ())
//         // count.Increment()
//         printfn $"{i} {i}"
//     printfn $"{count.Count}"

// Write here your unit test, you can later move it
// to Fable.Tests project. For example:
// testCase "Addition works" <| fun () ->
//     2 + 2 |> equal 4
