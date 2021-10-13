module Graph

type Graph = Graph of Map<int, Set<int>>

type GraphType =
    | Eulerian
    | SemiEulerian
    | NonEulerian

let graph nodes connections =
    Seq.init
        nodes
        (fun n ->
            connections
            |> Seq.map
                (fun conn ->
                    let v1 = fst conn
                    let v2 = snd conn

                    if v1 = n then Some v2
                    elif v2 = n then Some v1
                    else None)
            |> Seq.choose id
            |> Set.ofSeq)
    |> Seq.mapi (fun i x -> (i, x))
    |> Map.ofSeq
    |> Graph

let isEulerian (Graph g) =
    g
    |> Map.toSeq
    |> Seq.filter (fun (_, s) -> Set.count s % 2 <> 0)
    |> Seq.length
    |> function
        | 0 -> Eulerian
        | 2 -> SemiEulerian
        | _ -> NonEulerian

let isTree (Graph g) =
    g
    |> Map.toSeq
    |> Seq.fold (fun acc (_, x) -> acc + Set.count x) 0
    |> (/) 2
    |> (=) (Map.count g - 1)
