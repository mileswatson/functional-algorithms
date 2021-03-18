module Sorting

open Xunit

let size = 1031;
let test = seq { for i in [0..(size-1)] -> i*509 % size } |> Seq.toArray
let sorted = seq { for i in [0..(size-1)] -> i } |> Seq.toList

[<Fact>]
let ``Unoptimised merge sort`` () =
    Assert.StrictEqual(sorted, test |> Sorting.mergeSortUnoptimized )

[<Fact>]
let ``Optimised merge sort`` () =
    Assert.StrictEqual(sorted, test |> Sorting.mergeSort )
