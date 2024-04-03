namespace global

[<AutoOpen>]
module Mixin =
    let debug = true
    let printfn fmt = Printf.kprintf (fun str ->
  // Output the formatted string if 'debug', otherwise do nothing
        if debug then printfn "%s" str) fmt
