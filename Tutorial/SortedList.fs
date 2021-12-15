module SortedList

let findItemSortedArray arrayItem item =
    let rec internalFindItem (arrayItem: int array) item start ende =
        if start = ende then
            if arrayItem.[start] = item then
                Some start
            else
                None
        elif ende = start + 1 then
            if arrayItem.[start] = item then
                Some start
            elif arrayItem.[ende] = item then
                Some ende
            else
                None
        else
            let mid = (start + ende) / 2
            if arrayItem.[mid] < item then
                internalFindItem arrayItem item mid ende
            else
                internalFindItem arrayItem item start mid

    internalFindItem arrayItem item 0 (arrayItem.Length - 1)

let findItem arrayItem item =
    findItemSortedArray (Array.sort arrayItem) item