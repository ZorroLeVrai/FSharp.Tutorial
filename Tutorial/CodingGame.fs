module CodingGame

(* HANDLING INPUTS *)

let captureValuesWithArray inputSequence =
    let output = Array.create (Seq.length inputSequence) ""
    let mutable index = 0
    for input in inputSequence do
        output.[index] <- input
        index <- index + 1
    output

let captureValuesWithList inputSequence =
    let mutable output = []
    for input in inputSequence do
        output <- input::output
    output
    |> List.rev
    |> List.toArray

let captureValuesCompact inputSequence =
    inputSequence
    |> Seq.fold (fun acc item -> item::acc) []
    |> List.rev

(* CONVERTING ASCII TO STRING *)
let asciiToString (byteArr: byte array) =
    System.Text.ASCIIEncoding.ASCII.GetString(byteArr)

let stringToAscii (str: string) =
    System.Text.ASCIIEncoding.ASCII.GetBytes(str)

(* CONVERTING STRING TO ARRAY OF CHAR *)
let toCharArray str =
    str
    |> Seq.toArray

let toString (charArr: char array) =
    System.String.Concat(charArr)

(* RETURNING THE LAST ITEM OF A LIST *)
let rec getLastItem lst =
    match lst with
    | [x] -> x
    | x::xs -> getLastItem xs
    | _ -> failwith "Empty list"
