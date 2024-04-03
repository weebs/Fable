namespace global

[<AutoOpen>]
module Imports =

    // let printfn format =

        // let stack =
        //     match System.Environment.StackTrace.Split("\n") with
        //     | arr when arr.Length > 3 ->
        //         arr |> Array.skip 2 |> Array.take 2 |> Array.map (fun i -> ("    " + (i.Split("(")[0])))
        //     | _ -> [||]
        // Printf.kprintf (fun s -> System.Console.WriteLine s; for i in stack do System.Console.WriteLine i) format

    type print =
        // static member printfn (format: obj)  = ()
        static member printfn (format: obj) = ()
        // static member printfn (s: string) = ignore
    module Console =
        let WriteLine (s: string) =
            for i in 1..20 do System.Console.Write "=="
            // System.Console.WriteLine (System.Environment.StackTrace)
            System.Console.WriteLine ("\n" + s)
    let printfn (s: string) =
        Console.WriteLine s
    module io =
        let files = System.Collections.Concurrent.ConcurrentDictionary<string, string>()
        type file =
            static member AppendAllText (path: string, text: string) =
                 if files.ContainsKey path
                 then files[path] <- files[path] + text
                 else files[path] <- text
            static member write (path: string, text: string) = files[path] <- text
            static member Delete path =
                // files.TryRemove(path) |> ignore

                ()
            static member Exists path = files.ContainsKey path
        module path =
            let join (dir: string , path: string) = dir + "/" + path

