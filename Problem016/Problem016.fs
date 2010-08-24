let bigNumber pow =
     bigint.Pow(new bigint(2), pow).ToString()
     |> Seq.map(fun c -> System.Int32.Parse(c.ToString()))
     |> Seq.sum

printfn "answer = %d" (bigNumber 1000)