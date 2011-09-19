open System 
open System.Collections.Generic 

let sieve n = 
    seq { 
        yield 2 
        let knownComposites = new HashSet<int>() 
        for i in 3 .. 2 .. n do 
            let found = knownComposites.Contains(i) 
            if not found then 
                yield i 
            do for j in i .. i .. n do 
                   knownComposites.Add(j) |> ignore 
    }

let primes = sieve 1000000 |> Seq.toArray

let primeCounter n = 
    seq {
        for i in 1..50 do 
            let count =
                primes
                |> Seq.windowed i
                |> Seq.filter(fun x -> x |> Array.sum = n)
                |> Seq.map(fun x -> x |> Array.length)
            if count |> Seq.isEmpty then yield 0
            else yield count |> Seq.max
    }
    |> Seq.max

let answer = 
    primes
    |> Array.maxBy(primeCounter)

printfn "%i" answer