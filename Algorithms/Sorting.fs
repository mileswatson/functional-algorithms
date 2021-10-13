module Sorting

let rec mergeSortUnoptimized arr =

    let rec merge arr1 arr2 =
        match (arr1, arr2) with
        | ([], _) -> arr2
        | (_, []) -> arr1
        | (first1 :: tail1, first2 :: tail2) ->
            if first1 < first2 then
                merge tail1 arr2 |> List.append [ first1 ]
            else
                merge arr1 tail2 |> List.append [ first2 ]

    let rec msort arr =
        if Array.length arr = 1 then
            [ arr.[0] ]
        else
            let mid = (arr.Length - 1) / 2
            let left = mergeSortUnoptimized arr.[..mid]
            let right = mergeSortUnoptimized arr.[mid + 1..]
            merge left right

    msort arr



let mergeSort arr =

    let rec merge arr1 arr2 cont =
        match (arr1, arr2) with
        | ([], arr2) -> cont arr2
        | (arr1, []) -> cont arr1
        | (first1 :: tail1, first2 :: tail2) ->
            if first1 < first2 then
                merge tail1 arr2 (List.append [ first1 ] >> cont)
            else
                merge arr1 tail2 (List.append [ first2 ] >> cont)

    let rec msort arr cont =
        if Array.isEmpty arr then
            cont []
        else if Array.length arr = 1 then
            cont [ arr.[0] ]
        else
            let mid = (arr.Length - 1) / 2
            let left = arr.[..mid]
            let right = arr.[mid + 1..]
            msort left (fun leftSorted -> msort right (fun rightSorted -> merge leftSorted rightSorted id |> cont))

    msort arr id
