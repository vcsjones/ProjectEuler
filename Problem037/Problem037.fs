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

let primes = sieve 1000000 |> Seq.toList
let primeHashset = new HashSet<int>(primes)

let truncates n =
   let str = n.ToString()
   let rec loopLeft (s:string) = 
      match s with 
      | "" -> true
      | n when not(primeHashset.Contains(int n)) -> false
      | _ -> loopLeft(s.[.. s.Length-2])
   if not(loopLeft str) then false
   else
      let rec loopRight (s:string) = 
         match s with 
         | "" -> true
         | n when not(primeHashset.Contains(int n)) -> false
         | _ -> loopRight(s.[1..])
      if not(loopRight str) then false
      else true

let answer = 
   primes
   |> Seq.skipWhile(fun x -> x < 10)
   |> Seq.filter truncates
   |> Seq.take 11
   |> Seq.sum

printfn "answer = %d" answer