module FizzBuzz

let fizzBuzz start ende =
    let transformFizzBuzz n =
        let isDividedBy numerator denominator = (numerator % denominator) = 0

        let numberIsDividedBy3 = isDividedBy n 3
        let numberIsDividedBy5 = isDividedBy n 5
        match numberIsDividedBy3, numberIsDividedBy5 with
        | true, true -> "FizzBuzz"
        | true, false -> "Fizz"
        | false, true -> "Buzz"
        | _ -> string n

    seq { start .. ende }
    |> Seq.map transformFizzBuzz