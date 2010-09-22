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

let primes = sieve 1000000

let check n = 
   async {
      return Some(n, 0)
   }

let answer = 
   primes
   |> Seq.map check
   |> Async.Parallel
   |> Async.RunSynchronously
   |> Seq.choose(fun x -> x)
   |> Seq.maxBy snd
   |> fst

printfn "answer = %d" answer