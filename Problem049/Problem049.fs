open System.Collections.Generic 

let isPrime n = [|2.. (n|>float|>sqrt|>int)|] |> Array.exists(fun y -> n <> y && n % y=0) |> not

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

let arePermutations x y z =
   let difference = y - x
   if x = y || y = z || x = z then false
   elif z - y <> difference then false
   else
      let sortint n = new string(n.ToString() |> Seq.sort |> Seq.toArray)
      let xSort = sortint x
      let ySort = sortint y
      let zSort = sortint z
      xSort = ySort && ySort = zSort

let primes = sieve 9999 |> Seq.filter(fun x -> x > 1000) |> Seq.cache

let solutions = seq {
   for x in primes do
      for y in primes |> Seq.filter(fun f -> f > x) do
            let difference = y - x
            let z = y + difference
            if isPrime z && arePermutations x y z then yield (x,y,z)
   }

let str (x,y,z) = string x + string y + string z

let answer = 
   solutions
   |> Seq.filter(fun (x, y, z) -> x <> 1487)
   |> Seq.head
   |> str

printfn "answer = %s" answer
