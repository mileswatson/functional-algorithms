module Searching

open Xunit

let size = 17
let targetIndex = 4
let testArr = Array.init size (fun x -> x*x + 2*x + 1)

[<Fact>]
let ``Linear search`` () =
    let target = testArr.[targetIndex]
    Assert.StrictEqual(targetIndex, Searching.linearSearch testArr target)

[<Fact>]
let ``Binary search`` () =
    let target = testArr.[targetIndex]
    Assert.StrictEqual(targetIndex, Searching.binarySearch testArr target)
