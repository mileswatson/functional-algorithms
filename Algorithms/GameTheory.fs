module GameTheory

type Game = Game of double[,]

let rowPlaySafe (Game g) =
    seq { for i in 0..(Array2D.length1 g - 1) -> g.[i, *] }
    |> Seq.map (Seq.reduce min)
    |> Seq.reduce max

let columnPlaySafe (Game g) =
    seq {for i in 0..(Array2D.length2 g - 1) -> g.[*, i] }
    |> Seq.map (Seq.reduce max)
    |> Seq.reduce min

let stable g =
    (rowPlaySafe g) = (columnPlaySafe g)

let optimiseRows (Game g) =
    let strategies = seq { for i in 0..(Array2D.length1 g - 1) -> g.[i, *] }
    strategies
    |> Seq.mapi (fun i subject -> 
        strategies
        |> Seq.mapi (fun j dominant ->
            if i = j then false
            else
                Array.map2 (fun x y -> x > y) subject dominant
                |> Array.exists (fun x -> x)
                |> not )
        |> Seq.exists (fun x -> x) )
    |> Seq.map2 (fun x remove -> if remove then None else Some x) strategies 
    |> Seq.choose (fun x -> x)
    |> array2D
    |> Game
