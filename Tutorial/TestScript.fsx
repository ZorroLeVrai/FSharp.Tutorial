let AntSequence n =
    let nextSequence lst =
        let rec internalNextSequence lst counter prev res =
            let addCounterAndItem lst counter prev =
                if counter > 0 then
                    prev::counter::lst
                else
                    lst

            match lst with
            | [] -> addCounterAndItem res counter prev |> List.rev
            | x::xs -> match x with
                | x when x=prev -> internalNextSequence xs (counter+1) prev res
                | x -> internalNextSequence xs 1 x (addCounterAndItem res counter prev)
        internalNextSequence lst 0 -1 []

    let rec ant lst n = 
        match n with
        | 0 -> lst
        | n -> ant (nextSequence lst) (n-1)

    ant [1] n
    |> List.map string
    |> String.concat ""