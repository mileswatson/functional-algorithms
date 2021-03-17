module Searching

let linearSearch arr item =
    Array.findIndex ((=) item) arr

let rec private _binarySearch (arr: 'a array) item first last =
    if first = last then
        raise (System.Collections.Generic.KeyNotFoundException())
    else
        let mid = (first + last) / 2
        if (item < arr.[mid]) then
            _binarySearch arr item first mid
        elif (arr.[mid] < item) then
            _binarySearch arr item (mid+1) last
        else
            mid

let binarySearch arr item =
    _binarySearch arr item 0 arr.Length
