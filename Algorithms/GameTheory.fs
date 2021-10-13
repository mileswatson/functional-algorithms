module GameTheory

type Game = Game of double [,]

let rowPlaySafe (Game g) =
    seq { for i in 0 .. (Array2D.length1 g - 1) -> g.[i, *] }
    |> Seq.map (Seq.reduce min)
    |> Seq.reduce max

let columnPlaySafe (Game g) =
    seq { for i in 0 .. (Array2D.length2 g - 1) -> g.[*, i] }
    |> Seq.map (Seq.reduce max)
    |> Seq.reduce min

let stable g = (rowPlaySafe g) = (columnPlaySafe g)

let optimiseRows (Game g) =
    let strategies =
        seq { for i in 0 .. (Array2D.length1 g - 1) -> g.[i, *] }

    strategies
    |> Seq.mapi
        (fun i subject ->
            strategies
            |> Seq.mapi
                (fun j dominant ->
                    if i = j then
                        false
                    else
                        Array.map2 (>) subject dominant
                        |> Array.exists id
                        |> not)
            |> Seq.exists id)
    |> Seq.map2 (fun x remove -> if remove then None else Some x) strategies
    |> Seq.choose id
    |> array2D
    |> Game

let rowsMixedStrategy (Game g) =
    if Array2D.length1 g <> 2 || Array2D.length2 g <> 2 then
        invalidArg (nameof g) "Game must be 2x2"
    else
        let a = g.[0, 0]
        let b = g.[0, 1]
        let c = g.[1, 0]
        let d = g.[1, 1]
        (d - c) / (a - b - c + d)

let columnsMixedStrategy (Game g) =
    if Array2D.length1 g <> 2 || Array2D.length2 g <> 2 then
        invalidArg (nameof g) "Game must be 2x2"
    else
        let a = g.[0, 0]
        let b = g.[0, 1]
        let c = g.[1, 0]
        let d = g.[1, 1]
        (d - b) / (a - c - b + d)
