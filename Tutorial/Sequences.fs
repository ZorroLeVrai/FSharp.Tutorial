module Sequences

///Exercice 1

let incrementalSequence start ende = { start .. ende }

/// Exercice 2

let squareSequence start ende = seq { for i in start .. ende -> i*i }

let sumSquare start ende =
    squareSequence start ende
    |> Seq.sum

///Exercice 3

let triangleSequence n =
    1
    |> Seq.unfold (fun n -> Some(n*(n+1)/2, n+1))
    |> Seq.take n    

/// Exercice 4

let fibonacciSequence n =
    //input initial states a and b
    //output (next element, next state)
    Seq.unfold (fun (a,b) -> Some(a+b, (b, a+b))) (1,0)
    |> Seq.take n

//compute the fibonacci number
let rec simpleFibonacci = function
    | 0 -> 0
    | 1 -> 1
    | n -> simpleFibonacci (n-1) + simpleFibonacci (n-2)

let tailRecFibonacci n =
    let rec internalFibo n pprev prev =
        if n > 1 then
            internalFibo (n-1) prev (prev+pprev)
        else
            prev
    match n with
    | 0 -> 0
    | 1 -> 1
    | n -> internalFibo n 0 1
    
/// Exercice 5

let primesUpTo n =
    let rec sieve lst =
        match lst with
        | [] -> []
        | p::xs -> p :: sieve (List.filter (fun x -> x % p > 0) xs)
    [2..n] |> sieve

let primesUpTo2 n =
    let rec keepPrimes lst acc =
        match lst with
        | [] -> List.rev acc
        | x::xs -> if List.exists (fun item -> x % item = 0) acc then keepPrimes xs acc else keepPrimes xs (x::acc)

    keepPrimes [2..n] []
