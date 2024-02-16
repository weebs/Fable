let add = fun x y -> (x + y)

let printfn = fun value -> (System.Console.WriteLine (string value))

let mul = fun x y -> (x * y)

let main = fun (() : unit) ->
    let n = (add 2 3)
    let values = [| 1; 2; 3; 4 |]

    let n2 = (mul 4 20)
    System.Diagnostics.Debugger.Break()
    let f = fun (() : unit) -> (printfn n)
    let f2 = fun (() : unit) -> (printfn n)
    (0 |> ignore)
    let f2 = fun (() : unit) -> (printfn n)
    (0 |> ignore)
    for i in 1 .. 10 do
        (printfn i)
        (printfn i)
    420

main ()


