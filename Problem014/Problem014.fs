let rec collatz n =
   if n=1L then 1
   elif n%2L=0L then collatz(n/2L) + 1
   else collatz(3L*n+1L) + 1

let answer =
    Async.Parallel [ for i in 13L..999999L -> async { return (i, collatz i) } ]
    |> Async.RunSynchronously
    |> Seq.maxBy snd
printfn "answer = %d steps = %d" (fst answer) (snd answer)