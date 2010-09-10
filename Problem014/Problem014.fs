let collatz n =
   let rec loop(n,x) = 
      match n with
      | n when n = 1L -> x
      | n when n%2L = 0L -> loop(n/2L,x+1)
      | _ -> loop(3L*n+1L,x+1)
   loop(n,1)

let answer =
    Async.Parallel [ for i in 13L..999999L -> async { return (i, collatz i) } ]
    |> Async.RunSynchronously
    |> Seq.maxBy snd
printfn "answer = %d steps = %d" (fst answer) (snd answer)