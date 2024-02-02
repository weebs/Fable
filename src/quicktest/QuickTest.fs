// let anotherEffect =
//     fun () ->
//         printfn "set up"
//         fun () ->
//             printfn "clean up"

// let myEffect2 () () =
    // printfn "myEffect2"

let myEffect (b: bool) (i: int) =
    printfn "Effect!"
    fun (s: string) -> printfn "Cleaning up"

// let returnsEffect () =
//     myEffect
//
// let rec returnsReturnEffect () =
//     returnsEffect
//
// let useEffect3 (effect: unit -> unit -> (unit -> unit -> unit)): unit =
//     let fn = effect () () ()
//     fn ()
// let useEffect2 (effect: unit -> (unit -> unit -> unit)): unit =
//     let fn = effect () ()
//     let fn2 = effect ()
//     let fn1 = fn2 ()
//     fn ()
//     fn2 () ()
//     fn1 ()
let useEffect (effect: bool -> int -> (string -> unit)): unit =
    // Invoke
    effect true 420 ""
    // Apply
    let fn = effect true 1234
    printfn "callback"
    let s = ""
    printfn $"The effect is {fn} {fn s}"

    fn "hi"

let effects () =
    // let fn = myEffect ()
    // fn ()
    useEffect myEffect
    // useEffect3 returnsReturnEffect
    // useEffect2 returnsEffect
    // useEffect2 (fun foo -> fun () -> fun () -> ())
    // useEffect (fun foo -> returnsEffect foo ())
    // useEffect myEffect2
