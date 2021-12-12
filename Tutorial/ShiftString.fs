module ShiftString
(* Shift a string either to the left or to the right *)

let leftShift = function
    | [] -> []
    | x::xs -> xs @ [x]

let rightShift lst =
    lst
    |> List.rev
    |> leftShift
    |> List.rev

let completeShift (shiftFunc: char list -> char list) (str:string) =
    let charArr =
        str
        |> Seq.toList
        |> shiftFunc
        |> List.toArray
    System.String.Concat(charArr)

let change str command =
    match command with
    | "L" -> completeShift leftShift str
    | "R" -> completeShift rightShift str
    | _ -> failwith "Invalid command"
