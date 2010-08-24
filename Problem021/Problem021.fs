let d n = ([1L..n|>float|>sqrt|>int64]
             |> Seq.filter(fun x -> n%x=0L)
             |> Seq.map(fun x -> x+n/x)
             |> Seq.sum) - n

let answer =
         seq { for x in [1L..9999L] do 
               yield! Async.Parallel [ for y in [1L..9999L]  -> async { return ((x + y), x <> y && d(x) = d(y)) } ] |> Async.RunSynchronously
         }
         |> Seq.filter(fun (x, y) -> y)
         |> Seq.map(fun (x, y) -> x)
         |> Seq.sum

printfn "answer = %d" answer