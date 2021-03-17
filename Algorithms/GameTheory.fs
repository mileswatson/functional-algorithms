module GameTheory

type Game = Game of double[,]

let game x = Game x

let rowPlaySafe (Game g) =
    seq { for i in 0..(Array2D.base1 g) -> g.[i, *] }
    |> Seq.map (Seq.reduce min)
    |> Seq.reduce max

let columnPlaySafe (Game g) =
    seq {for i in 0..(Array2D.base2 g) -> g.[*, i] }
    |> Seq.map (Seq.reduce max)
    |> Seq.reduce min

let stable g =
    (rowPlaySafe g) = (columnPlaySafe g)
