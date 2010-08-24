let isPrime n = [|2L.. (n|>float|>sqrt|>int64)|] |> Array.exists(fun y -> n <> y && n % y=0L) |> not

let answer = 
   seq {
         yield 2L
         yield!
            Async.Parallel [ for i in [3L..2L..2000000L] -> async { return (i, isPrime(i)) } ]
            |> Async.RunSynchronously
            |> Seq.filter snd
            |> Seq.map fst

      }
   |> Seq.sum

printfn "answer = %d" answer