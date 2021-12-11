// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Sequences

let printSequence s =
    s
    |> Seq .iter (printf "%i ")

[<EntryPoint>]
let main argv =
    printSequence (AntSequence 3)
    0 // return an integer exit code  