// // For more information see https://aka.ms/fsharp-console-apps
module Fable.Tests.C.Program
//
// open System.Collections.Generic
// // todo: doesn't work, value isn't initialized
// // todo: how to handle ref count on static objs?
// // let staticDictionary = Dictionary<int, string>()
// let staticList = List<string>()
//
// let collections_test () =
//     // let i = System.Collections_Generic.List<int>()
//     // let i2 = System.Collections_Generic.Dictionary<int, string>()
//     // let i3 = System.Collections_Generic.Dictionary<string, string>()
//     let d = Dictionary<int, string>()
//     // todo: this will only work with interning or if GetHashCode calls the right method
//     let d2 = Dictionary<string, string>()
//     let l = List<int>()
//     l.Add(420)
//     d.Add(1, "1")
//     printfn "Hello from F#"
//     // let hash = Fable.Hash.hashValue (nativeint (unativeint 1234)) 8
//     printfn $"{d[1]}"
//     printfn $"{l[0]}"
//
// let constValue = 0
// let lambda_test () =
//     let rec f value =
//         if value = 0 then f (value + 1)
//         else value + constValue + 1
//     printfn $"{f 43}"
//
// // type DU<'t> =
// type DU =
//     | A of foo: string
//     | B of int * float
//     | C of List<string>
//
//
// let caseA value = DU.A value
// let duTest () =
//     let du = caseA "420"
//     let du2 = DU.B (42, 1)
//     let du3 = DU.C (List<string>())
//     match du with
//     | DU.A foo ->
//         match du2 with
//         | DU.A foo ->
//             printfn $"{foo}"
//         | DU.B (n, n1) ->
//             printfn $"{n}"
//         | DU.C _ -> ()
//         printfn $"{foo}"
//     | DU.B (n, n1) ->
//         match du2 with
//         | DU.A foo ->
//             printfn $"{foo}"
//         | DU.B (n, n1) ->
//             printfn $"{n}"
//         | DU.C _ -> ()
//         printfn $"{n}"
//     | DU.C s -> printfn ""
//     // let du3 = DU.C 420.69
//     printfn $"{du} {du2}"
//
// let add_nums a b c : int = a + b + c
// // let result =
// //     let foo =
// //         add_nums
// //             3
// //             (add_nums 2 (let a = 10 in let mutable count = 0 in (while count < a do count <- count + 1); count) 8)
// //             (let a = 10 in let mutable count = 0 in (while count < a do count <- count + 1); count)
// //     foo * 2
// collections_test ()
// duTest ()
// printfn "yo"
//
// open System
//
// /// Type that represents Success/Failure in parsing
// type ParseResult<'a> =
//   | Success of 'a
//   | Failure of string
//
// /// Type that wraps a parsing function
// type Parser<'T> = Parser of (string -> ParseResult<'T * string>)
//
// /// Parse a single character
// let pchar charToMatch =
//   // define a nested inner function
//   let innerFn str =
//     if String.IsNullOrEmpty(str) then
//       Failure "No more input"
//     else
//       let first = str.[0]
//       if first = charToMatch then
//         let remaining = str.[1..]
//         Success (charToMatch,remaining)
//       else
//         // let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
//         let msg = $"Expecting '{charToMatch}'. Got '{first}'"
//         Failure msg
//   // return the "wrapped" inner function
//   Parser innerFn
//
// /// Run a parser with some input
// let run parser input =
//   // unwrap parser to get inner function
//   let (Parser innerFn) = parser
//   // call inner function with input
//   innerFn input
open Fable.Tests.C
Fable.Tests.C.Closures.test ()
Fable.Tests.C.Objects.Records.test ()
Fable.Tests.C.Structs.test ()
Fable.Tests.C.Options.test ()
Fable.Tests.C.Tuples.test ()
Fable.Tests.C.Unions.test ()
Fable.Tests.C.Collections.test ()
Byrefs.tests ()
