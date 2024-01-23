module QuickTest.Generics

type Vec<'t>() =
    let mutable items = [| Unchecked.defaultof<_> |]
    let mutable n = 0
    let mutable size = 1

    member this.Add(item: 't) =
        if n >= size then
            let oldItems = items
            size <- size * 2
            items <- Array.zeroCreate (size * 2)

            for i in 0 .. n - 1 do
                items[i] <- oldItems[i]

        items[n] <- item
        n <- n + 1
