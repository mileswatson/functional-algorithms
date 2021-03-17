module Tests

open System
open Xunit

let stableGame =
    [[ 2.; 3. ]
     [ -4.; 5.]]
    |> array2D
    |> GameTheory.game

[<Fact>]
let ``Check rowPlaySafe (maximin)`` () =
    let maximin =
        stableGame
        |> GameTheory.rowPlaySafe
    Assert.StrictEqual(2., maximin)

[<Fact>]
let ``Check columnPlaySafe (minimax)`` () =
    let minimax =
        stableGame
        |> GameTheory.columnPlaySafe
    Assert.StrictEqual(2., minimax)

[<Fact>]
let ``Check stable game`` () =
    Assert.True(GameTheory.stable stableGame)

