let answer = seq {
                     for a in [2I..100I] do
                        for b in [2..100] do
                           yield a**b
                 }
                 |>Seq.distinct
                 |>Seq.length

printfn "answer = %d" answer