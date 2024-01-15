module System

module Hash =
    let hashValue (pointer: nativeint) (length: int) : int =
        let pointer: nativeptr<byte> = NativeInterop.NativePtr.ofNativeInt pointer
        let mutable hash = 420
        let M = 69
        for i in 0..length - 1 do
            let value = NativeInterop.NativePtr.get pointer i
            hash <- (M * hash) + int value
        hash

module Collections_Generic =
    type List<'t>() =
        let mutable items: 't[] = Array.zeroCreate 0
        member this.Add(item: 't) =
            printfn $"{item}"
    type Dictionary<'key, 'value when 'key: equality>() =
        // let items: 'value[] = Array.zeroCreate 0
        let buckets: ('key * 'value)[][] = Array.zeroCreate 0
        // let keys: 'key[][] = Array.zeroCreate 0
        // let buckets: 'value[][] = Array.zeroCreate 0
        member this.Item
            with get (key: 'key) : 'value =
                let hash = key.GetHashCode()
                let bucketIndex = hash % buckets.Length
                let mutable found = false
                let mutable index = 0
                while found && index < buckets[bucketIndex].Length do
                    let value: 'key = fst (buckets[bucketIndex][index])
                    // let value: 'key = keys[bucketIndex][index]
                    if value = key then
                        found <- true
                    index <- index + 1
                snd (buckets[bucketIndex][index - 1])
        member this.Add (key: 'key, value: 'value) =
            // let mutable k = key
            // let hash = Hash.hashValue (NativeInterop.NativePtr.toNativeInt &&k) sizeof<'key>
            let hash = key.GetHashCode()
            ()
