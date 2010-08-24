let answer =  seq {for x in 1..999 do if x%5=0 || x%3=0 then yield x} |> Seq.sum
printfn "answer = %d" answer