let factorial n = [1UL .. n] |> Seq.fold(fun acc n -> acc*n) 1UL

let factDigits n = 
   n.ToString().ToCharArray()
   |> Array.map(fun x -> int(x) - 0x30)
   |> Array.fold(fun acc x -> factorial (uint64(x)) + acc) 0UL

let countFactorial n = 
   let factChain =  Set.singleton n
   let rec loop x l = 
      if l |> Set.count > 60 then
         0
      else
         let factDigit = factDigits x
         if l |> Set.exists(fun y -> y=factDigit) then
            l |> Set.count
         else
            let newChain = l + Set.singleton factDigit
            loop factDigit newChain
   loop n factChain

let answer = 
   Async.Parallel [ for i in 1UL..999999UL -> async { return countFactorial i } ]
   |> Async.RunSynchronously
   |> Seq.filter(fun x -> x = 60)
   |> Seq.length

printfn "answer = %d" answer