module Fable.Tests.ResizeArrayTests

open Util.Testing

[<Fact>]
let ``ResizeArray zero creation works`` () =
    let li = ResizeArray<float>()
    li.Count |> equal 0

// [<Fact>]
// let ``ResizeArray zero creation with size works`` () =
//     let li = ResizeArray<string>(5)
//      li.Count |> equal 0

// [<Fact>]
// let ``ResizeArray creation with seq works`` () =
//     let li = ResizeArray<_>(seq{1..5})
//     Seq.sum li |> equal 15

// [<Fact>]
// let ``ResizeArray creation with literal array works`` () =
//     let li = ResizeArray<_> [|1;2;3;4;5|]
//     Seq.sum li |> equal 15

// [<Fact>]
// let ``ResizeArray creation with literal list works`` () =
//     let li = ResizeArray<_> [1;2;3;4;5]
//     Seq.sum li |> equal 15

// [<Fact>]
// let ``ResizeArray casting to seq works`` () =
//     let xs = ResizeArray<_>(seq{1..5}) :> seq<_>
//     Seq.sum xs |> equal 15

// [<Fact>]
// let ``ResizeArray iteration works`` () =
//     let li = ResizeArray<_>()
//     li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
//     let mutable acc = 0.
//     for i in li do
//         acc <- acc + i
//     acc |> equal 15.

// [<Fact>]
// let ``ResizeArray iteration with index works`` () =
//     let li = ResizeArray<_>()
//     for i = 1 to 4 do
//         li.Add(i)
//     let mutable x = 0
//     for i = 0 to li.Count - 1 do
//         x <- x + li.[i]
//     x |> equal 10

// [<Fact>]
// let ``ResizeArray folding works`` () =
//     let li = ResizeArray<_>()
//     li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
//     li |> Seq.fold (fun acc item -> acc + item) 0.
//     |> equal 15.

[<Fact>]
let ``ResizeArray.Count works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    li.Count |> equal 5

// [<Fact>]
// let ``ResizeArray.ConvertAll works`` () =
//     let xs = ResizeArray<_> [1.; 2.; 3.; 4.]
//     let ys = xs.ConvertAll(System.Converter(fun x -> int x))
//     ys |> Seq.toList |> equal [1;2;3;4]

// [<Fact>]
// let ``ResizeArray.Find works`` () =
//     let li = ResizeArray<_>()
//     li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
//     System.Predicate<_> (fun x -> x = 1.)  |> li.Find |> equal 1.
//     System.Predicate<_> (fun x -> x = -1.) |> li.Find |> equal 0.

// [<Fact>]
// let ``ResizeArray.Find with option works`` () =
//     let li = ResizeArray<_>()
//     li.Add(Some 1); li.Add(None);
//     System.Predicate<_> (fun _ -> true)  |> li.Find |> equal (Some 1)
//     System.Predicate<_> (fun _ -> false)  |> li.Find |> equal None
//     System.Predicate<_> Option.isNone  |> li.Find |> equal None
//     System.Predicate<_> Option.isSome  |> li.Find |> equal (Some 1)

// [<Fact>]
// let ``ResizeArray.FindAll works`` () =
//     let xs = ResizeArray<_> [1.; 2.; 3.; 4.]
//     System.Predicate<_> (fun x -> x <= 3.) |> xs.FindAll |> (fun l -> l.Count) |> equal 3
//     System.Predicate<_> (fun x -> x = 5.) |> xs.FindAll |> (fun l -> l.Count) |> equal 0

// [<Fact>]
// let ``ResizeArray.FindLast works`` () =
//     let li = ResizeArray<_>()
//     li.Add(1.,0.); li.Add(2.,0.); li.Add(3.,0.); li.Add(4.,0.); li.Add(5.,0.); li.Add(1.,1.)
//     System.Predicate<_> (fun (x, _) -> x = 1.)  |> li.FindLast |> snd |> equal 1.

// [<Fact>]
// let ``ResizeArray.FindLast with option works`` () =
//     let li = ResizeArray<_>()
//     li.Add(Some 1); li.Add(None);
//     System.Predicate<_> (fun _ -> true)  |> li.FindLast |> equal None
//     System.Predicate<_> (fun _ -> false)  |> li.FindLast |> equal None
//     System.Predicate<_> Option.isSome  |> li.FindLast |> equal (Some 1)

// [<Fact>]
// let ``ResizeArray.FindIndex works`` () =
//     let li = ResizeArray<_>()
//     li.Add(1.); li.Add(2.); li.Add(3.); li.Add(2.); li.Add(5.)
//     System.Predicate<_> (fun x -> x = 2.) |> li.FindIndex |> equal 1
//     System.Predicate<_> (fun x -> x = 0.) |> li.FindIndex |> equal -1

// [<Fact>]
// let ``ResizeArray.FindLastIndex works`` () =
//     let li = ResizeArray<_>()
//     li.Add(1.); li.Add(2.); li.Add(3.); li.Add(2.); li.Add(5.)
//     System.Predicate<_> (fun x -> x = 2.) |> li.FindLastIndex |> equal 3
//     System.Predicate<_> (fun x -> x = 0.) |> li.FindLastIndex |> equal -1

// [<Fact>]
// let ``ResizeArray.ForEach works`` () =
//     let li = ResizeArray<_>()
//     let mutable sum = 0
//     li.Add(1); li.Add(2); li.Add(3); li.Add(4); li.Add(5)
//     System.Action<_> (fun x -> sum <- sum + x) |> li.ForEach
//     sum |> equal 15

// [<Fact>]
// let ``ResizeArray indexer getter works`` () =
//     let li = ResizeArray<_>()
//     li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
//     equal 2. li.[1]

// [<Fact>]
// let ``ResizeArray indexer setter works`` () =
//     let li = ResizeArray<_>()
//     li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
//     li.[3] <- 10.
//     equal 10. li.[3]

[<Fact>]
let ``ResizeArray.Clear works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    li.Clear()
    li.Count |> equal 0

[<Fact>]
let ``ResizeArray.Add works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.)
    equal 3 li.Count

// [<Fact>]
// let ``ResizeArray.AddRange works`` () =
//     let li = ResizeArray<_>()
//     li.AddRange [1;2;3]
//     equal 3 li.Count

// [<Fact>]
// let ``ResizeArray.InsertRange works`` () =
//     let li = ResizeArray<_>()
//     let mutable sum = 0
//     li.Add(1); li.Add(2); li.Add(5)
//     li.InsertRange(2, [3;4])
//     Seq.toList li |> equal [1;2;3;4;5]

// [<Fact>]
// let ``ResizeArray.GetRange works`` () =
//     let li = ResizeArray<_>()
//     li.AddRange [1;2;3]
//     let sub = li.GetRange(1, 2)
//     sub.Count |> equal 2
//     sub.Contains(1) |> equal false

// [<Fact>]
// let ``ResizeArray.Contains works`` () =
//     let li = ResizeArray<_>()
//     li.Add("ab")
//     li.Add("ch")
//     li.Contains("ab") |> equal true
//     li.Contains("cd") |> equal false

// [<Fact>]
// let ``ResizeArray.IndexOf works`` () =
//     let li = ResizeArray<_>()
//     li.Add("ch")
//     li.Add("ab")
//     li.IndexOf("ab") |> equal 1
//     li.IndexOf("cd") |> equal -1

// [<Fact>]
// let ``ResizeArray.Remove works`` () =
//     let li = ResizeArray<_>()
//     li.Add("ab")
//     li.Add("ch")
//     li.Remove("ab") |> equal true
//     li.Remove("cd") |> equal false

// [<Fact>]
// let ``ResizeArray.RemoveAll works`` () =
//     let li = ResizeArray<_>()
//     li.Add("ab")
//     li.Add("ch")
//     li.Add("ab")
//     System.Predicate<_> (fun x -> x = "ab") |> li.RemoveAll |> equal 2
//     System.Predicate<_> (fun x -> x = "ab") |> li.RemoveAll |> equal 0
//     li.[0] |> equal "ch"

// [<Fact>]
// let ``ResizeArray.RemoveRange works`` () =
//     let xs = ResizeArray<int>()
//     for x in [1 .. 5] do xs.Add(x)
//     xs.RemoveRange(1, 2) // [1;2;3;4;5] -> [1;4;5]
//     equal 1 xs.[0]
//     equal 4 xs.[1]
//     equal 5 xs.[2]

// [<Fact>]
// let ``ResizeArray.Exists works`` () =
//     let xs = ResizeArray<int>()
//     for x in [1 .. 5] do xs.Add(x)
//     xs.Exists (fun a -> a > 5) |> equal false
//     xs.Exists (fun a -> a = 5) |> equal true
//     xs.Exists (fun a -> a > 1) |> equal true
//     xs.Exists (fun a -> a = 1) |> equal true
//     xs.Exists (fun a -> a < 1) |> equal false
//     xs.Exists (fun a -> a = 3) |> equal true

[<Fact>]
let ``ResizeArray.RemoveAt works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    li.RemoveAt(2)
    equal 4. li.[2]

// [<Fact>]
// let ``ResizeArray.Insert works`` () =
//     let li = ResizeArray<_>()
//     li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
//     li.Insert(2, 8.)
//     equal 8. li.[2]

// [<Fact>]
// let ``ResizeArray.ReverseInPlace works`` () =
//     let li = ResizeArray<_>()
//     li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
//     li.Reverse()
//     equal 2. li.[3]

// [<Fact>]
// let ``ResizeArray.SortInPlace works`` () =
//     let li = ResizeArray<_>()
//     li.Add("Ana"); li.Add("Pedro"); li.Add("Lucía"); li.Add("Paco")
//     li.Sort()
//     equal "Paco" li.[2]
//     let li2 = ResizeArray [1;3;10;2]
//     li2.Sort()
//     equal 2 li2.[1]

// [<Fact>]
// let ``ResizeArray.SortInPlaceWith works`` () =
//     let li = ResizeArray<_>()
//     li.Add(3.); li.Add(6.); li.Add(5.); li.Add(4.); li.Add(8.)
//     li.Sort(fun x y -> if x > y then -1 elif x < y then 1 else 0)
//     equal 4. li.[3]

// [<Fact>]
// let ``ResizeArray.SortInPlaceWith works with custom comparison function`` () = // See #1386
//     let ns = ResizeArray<int> [1;3;2]
//     ns.Sort(fun x y -> if x < y then 1 else -1)
//     Seq.toList ns |> equal [3; 2; 1]
//     ns.Sort(compare)
//     Seq.toList ns |> equal [1;2;3]

// [<Fact>]
// let ``ResizeArray.SortInPlaceWith works with custom comparer`` () = // See #1386
//     let ns = ResizeArray<int> [1;3;2]
//     let comparer = System.Collections.Generic.Comparer<int>.Default
//     ns.Sort(comparer)
//     Seq.toList ns |> equal [1;2;3]

// [<Fact>]
// let ``ResizeArray.ToArray works`` () =
//     let li = ResizeArray<_>()
//     li.Add(3.); li.Add(6.); li.Add(5.); li.Add(4.); li.Add(8.)
//     equal 5 li.Count
//     let ar = li.ToArray()
//     Array.length ar |> equal li.Count
//     ar.[0] <- 2.
//     equal 3. li.[0]
//     equal 2. ar.[0]

// [<Fact>]
// let ``ResizeArray.Item is undefined when index is out of range`` () =
//     let xs = ResizeArray [0]
//     #if FABLE_COMPILER
//     isNull <| box (xs.Item 1)
//     #else
//     try (xs.Item 1) |> ignore; false with _ -> true
//     #endif
//     |> equal true
