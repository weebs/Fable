module System

module Hash =
    let hashValue (pointer: nativeint) (length: int) : int =
        let pointer: nativeptr<byte> = NativeInterop.NativePtr.ofNativeInt pointer
        let mutable hash = 420
        let M = 69
        for i in 0..length - 1 do
            let value = NativeInterop.NativePtr.get pointer i
            hash <- (M * hash) + int value
        if hash < 0 then -1 * hash else hash

module Collections_Generic =
    type List<'t>() =
        let mutable items: 't[] = Array.zeroCreate 0
        member this.Add(item: 't) =
            printfn $"{item}"
        member this.Item
            with get index = items[index]
    type Dictionary<'key, 'value when 'key: equality>() =
        // let items: 'value[] = Array.zeroCreate 0
        let buckets: ('key * 'value)[][] = Array.zeroCreate 8
        let counts: int[] = Array.zeroCreate 8
        do
            for i in 0..7 do
                buckets[i] <- Array.zeroCreate 1
                counts[i] <- 0
        // let keys: 'key[][] = Array.zeroCreate 0
        // let buckets: 'value[][] = Array.zeroCreate 0
        member this.Item
            with get (key: 'key) : 'value =
                let hash = key.GetHashCode()
                let bucketIndex = hash % buckets.Length
                let mutable found = false
                let mutable index = 0
                while not found && index < counts[bucketIndex] do
                    let value: 'key = fst (buckets[bucketIndex][index])
                    // let value: 'key = keys[bucketIndex][index]
                    if value = key then
                        found <- true
                    index <- index + 1
                if not found then
                    ()
                    // todo: exit 1337
                snd (buckets[bucketIndex][index - 1])
        member this.Add (key: 'key, value: 'value) =
            // let mutable k = key
            // let hash = Hash.hashValue (NativeInterop.NativePtr.toNativeInt &&k) sizeof<'key>
            let hash = key.GetHashCode()
            printfn $"hash = {hash}"
            printfn $"hash index = {hash % buckets.Length}"
            let bucketIndex = hash % buckets.Length
            if counts[bucketIndex] >= buckets[bucketIndex].Length then
                let bucket = buckets[bucketIndex]
                let newBucket = Array.zeroCreate (bucket.Length * 2)
                for i in 0..(counts[bucketIndex]) - 1 do
                    newBucket[i] <- bucket[i]
                buckets[bucketIndex] <- newBucket
            buckets.[bucketIndex].[counts[bucketIndex]] <- (key, value)
            counts[bucketIndex] <- counts[bucketIndex] + 1
