let bigNumber pow =
     2I**pow
     |> string
     |> Seq.map(fun c -> int c - 0x30)
     |> Seq.sum

printfn "answer = %d" (bigNumber 1000)