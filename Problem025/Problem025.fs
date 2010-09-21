let fib = Seq.unfold(fun (p, c) -> Some((p, c), (c, p+c))) (1I,1I)
let answer = 
   fib
   |> Seq.mapi(fun i x -> (i+1, (fst x).ToString("R")))
   |> Seq.filter(fun (_, x) -> x.Length = 1000)
   |> Seq.head
   |> fst
printfn "answer = %d" answer