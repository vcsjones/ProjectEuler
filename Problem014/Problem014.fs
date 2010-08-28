let rec collatz n =
   match n with
   | n when n = 1L -> 1
   | n when n%2L = 0L -> collatz(n/2L) + 1
   | _ -> collatz(3L*n+1L) + 1

let answer =
    Async.Parallel [ for i in 13L..999999L -> async { return (i, collatz i) } ]
    |> Async.RunSynchronously
    |> Seq.maxBy snd
printfn "answer = %d steps = %d" (fst answer) (snd answer)