module AddSubstractInList

let addSub lst =
    let listToNbTimesDico lst =
        let addItemToDico dicoAcc item =
            let foundValue = Map.tryFind item dicoAcc
            match foundValue with
            | Some n -> Map.add item (n+1) dicoAcc
            | _ -> Map.add item 1 dicoAcc

        List.fold addItemToDico Map.empty lst

    let getValue key nbOccurences =
        match nbOccurences with
            | n when n % 2 = 0 -> -1*key
            | _ -> key

    listToNbTimesDico lst
    |> Map.fold (fun acc key nbOccurences ->  acc + (getValue key nbOccurences)) 0