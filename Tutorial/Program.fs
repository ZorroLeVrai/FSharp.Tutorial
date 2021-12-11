// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open FizzBuzz

let printSequence sequence printFunction =
    sequence
    |> Seq.map string
    |> Seq.iter printFunction

let printSequenceOneLine sequence =
    printSequence sequence (printf "%s ")

let printSequenceMultipleLine sequence =
    printSequence sequence (printfn "%s ")

[<EntryPoint>]
let main argv =
    printSequenceMultipleLine (fizzBuzz 1 30)
    0 // return an integer exit code  