let quad n a b = n*n + a*n + b

let isPrime n = n > 0 && [|2.. (n|>float|>sqrt|>int)|] |> Array.exists(fun y -> n <> y && n % y=0) |> not

let answer = 
   seq {
   for a in {-999..999} do
      for b in {1..999} do
         let primes = 
            Seq.unfold(fun x -> Some(x, x+1)) 0
            |> Seq.takeWhile(fun x -> isPrime (quad x a b))
         let len = primes |> Seq.length
         yield (len, a, b)
   }
   |> Seq.maxBy(fun (x, _, _) -> x)

let _, a, b = answer

printfn "a = %d; b = %d" a b