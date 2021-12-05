let rec sieve lst =
    match lst with
    |[] -> []
    |p::xs -> p :: sieve (List.filter (fun x -> x % p > 0) xs)

sieve [2..100]