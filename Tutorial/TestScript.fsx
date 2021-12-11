let AntSequence n =
    let next lst =
        let rec intNext lst counter prev res =
            let addPrev lst counter prev =
                if counter > 0 then
                    prev::counter::lst
                else
                    lst
            match lst with
            | [] -> addPrev res counter prev |> List.rev
            | x::xs -> match x with
                | x when x=prev -> intNext xs (counter+1) prev res
                | x -> intNext xs 1 x (addPrev res counter prev)
        intNext lst 0 -1 []

    let rec ant lst n = 
        match n with
        | 0 -> lst
        | n -> ant (next lst) (n-1)
    ant [1] n
    |> List.map string
    |> String.concat ""