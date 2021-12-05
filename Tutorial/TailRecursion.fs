module TailRecursion

let numbers = [1..100]

//simple recursion function where the result is appended to the return value
let rec even ls =
    match ls with
    |[] -> []
    |x::xs when x % 2 = 0 -> x :: (even xs)
    |_::xs -> even xs

//result 1
let evenNumbers = even numbers

let numbers' = [1..1000000]

//stack overflow exception: because appending each time to the returned value exceeds the stack memory
let evenNumbers2 = even numbers'

//tail recursion: the result is returned each time as a parameter - notice the additional parameter `acc`
let rec even' ls acc =
    match ls with
    |[] -> acc
    |x::xs when x % 2 = 0 -> even' xs (x::acc)
    |_::xs -> even' xs acc

//no stack overflow exception
let evenNumbers3 = even' numbers' []