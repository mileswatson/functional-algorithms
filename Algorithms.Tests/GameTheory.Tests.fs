module GameTheory

open Xunit

let stableGame =
    [[ 2.; 3. ]
     [ -4.; 5.]]
    |> array2D
    |> GameTheory.Game

let dominatedGame =
    [[ 2.; 3. ]
     [ 1.; 0. ]
     [ -4.; 5.]]
    |> array2D
    |> GameTheory.Game

let randomGame =
    [[ 4.; 1. ]
     [ -2.; 2.]]
    |> array2D
    |> GameTheory.Game

[<Fact>]
let ``Find rows' playsafe`` () =
    let maximin =
        dominatedGame
        |> GameTheory.rowPlaySafe
    Assert.StrictEqual(2., maximin)

[<Fact>]
let ``Check columns' playsafe`` () =
    let minimax =
        dominatedGame
        |> GameTheory.columnPlaySafe
    Assert.StrictEqual(2., minimax)

[<Fact>]
let ``Check stable game`` () =
    Assert.True(GameTheory.stable stableGame)

[<Fact>]
let ``Remove dominated rows`` () =
    Assert.StrictEqual(stableGame, GameTheory.optimiseRows dominatedGame)

[<Fact>]
let ``Rows mixed strategy``() =
    Assert.StrictEqual(4./7., GameTheory.rowsMixedStrategy randomGame)

[<Fact>]
let ``Columns mixed strategy``() =
    Assert.StrictEqual(1./7., GameTheory.columnsMixedStrategy randomGame)
