let answer = 
   seq {
   for a in [1.0..500.0] do
      for b in [1.0..500.0] do
         let c = 1000.0-a-b
         yield (a,b,c)
   } |> Seq.filter(fun (a,b,c) -> a**2.0 + b**2.0 = c **2.0)
     |> Seq.map(fun (a,b,c) -> a*b*c)
     |> Seq.head
     |> int

printfn "answer = %d" answer