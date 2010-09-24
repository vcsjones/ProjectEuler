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

let primes = sieve 999999 |> Seq.cache

let check n = 
   async {
      if n = 2 || n = 3 then return None
      else
         let primesToHalf = primes |> Seq.takeWhile(fun x -> x <= int(ceil(float n / 2.0)))
         let countPrimes start = 
            let rec loop(primeList,sum,count) = 
               match primeList with
               | head :: tail when head + sum = n -> Some(n, count)
               | head :: tail -> loop(tail,head+sum,count+1)
               | [] -> None
            loop(primesToHalf |> Seq.skipWhile(fun x -> x < start) |> Seq.toList, 0, 1)
         let primeCounts =
               primesToHalf
               |> Seq.map(fun x -> (x, countPrimes x))
               |> Seq.filter(fun (_, x) -> Option.isSome x)
         let result = 
            match primeCounts with
            | a when a |> Seq.isEmpty -> None
            | a -> a |> Seq.maxBy fst |> snd
         return result
   }


let answer = 
   primes
   |> Seq.map check
   |> Async.Parallel
   |> Async.RunSynchronously
   |> Seq.choose(fun x -> x)
   |> Seq.maxBy snd
   |> fst

printfn "answer = %A" answer