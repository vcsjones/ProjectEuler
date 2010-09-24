open System.Collections.Generic 

let isPandigital n = 
   let str = new string(n.ToString().ToCharArray() |> Array.sort)
   let matcher = [1..str.Length] |> Seq.map string |> Seq.reduce(fun acc x -> acc+x)
   str = matcher

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

let primes = sieve 10000000 |> Seq.cache

let answer = 
   primes
   |> Seq.filter isPandigital
   |> Seq.max

printfn "answer = %d" answer
