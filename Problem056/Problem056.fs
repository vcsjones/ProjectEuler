let answer = 
   seq {
         for a in [1I..99I] do
            for b in [1..99] do
               yield a**b
       }

   |> Seq.map(fun x -> x.ToString("R") |> Seq.sumBy(fun y -> int y - 0x30))
   |> Seq.max

printfn "answer = %d" answer