module Fone.Interp.Sources

let x , y = 1 , 2
// let takes3 x y z : int = x + y + z
// let print n =
//     // printfn $"{n}"
//     ()
// do
//     // print
//     //     takes3 1 2 3 // error
//     print
//         (takes3 1 2 3) // fine
//     let n =
//         takes3
//             1
//             2 3 // fine
//     let n2 =
//         takes3 1
//             2 3
//     print n
//     let f =
//         takes3 1
//           2
//         3
//     print f
let fn =
    for i
     in 1..10
     do
        printfn ""
        printfn ""
    fun
     ()
      ->
          ()
// let f2 = fun ()
//   ->
//     printfn n
let lambdas = """
let main () =
    let n = string 0
    let printfn (s: string) = System.Console.WriteLine s
    let f = fun () ->
        printfn n
    let f2 = fun ()
        ->
            printfn n
    0
    let f2 =
        fun () ->
            printfn n
    0
"""
let normalCode = """
let add3 x y z = (x + y) + z
let add2plus1 = add3 1
let three = add2plus1 1 1

let add (x: int) (y: int) = x + y
let add1 = add 1
let two = add1 1

type SourceRange start end data = {
    start: start; end: end;
    data: data
}
let SourceRangeInt = SourceRange @int
let foo = SourceRange @int @int @int { start = 0; end = 0; data = 420 }
let printfn value = System.Console.WriteLine (string value)
let mul x y =
    x * y
let main () =
    let n = add 2 3
    let values = [ 1; (add 2 3); 3; 4; ]
    let n2 =
        mul 4 20
    let f = fun () ->
        printfn n
    let f2 = fun ()
        ->
            printfn n
    0 |> ignore
    let f2 =
        fun () ->
            printfn n
    0 |> ignore
    for i
     in 1..10
     do
        printfn i
        printfn i
    420

let result = main ()
"""
let source = """
let foo (x: int) y =
  let asdf () =
    ()
  let fn =
      fun () ->
        fun () ->
            n <- n + 1
            n * 2
"""
let source2 = """
let foo (x: int) y =
  let asdf () =
    ()
  let fn =
      fun () ->
        fun () ->
            n <- n + 1
            n * 2
  let add3 x y z = x + y + z
  // add3 1
    // 2 3
  add3 1 2 3
  let nums = [ 1; 2; 3; 4 ]
  let n =
    a + c + d + e + f
    x + y
  n + x + y
  let n : int = 0
  foo
    foo x
     y z
    bar a b
      c
     """
