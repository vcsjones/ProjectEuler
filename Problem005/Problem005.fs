let answer = 
   Seq.unfold(fun x -> Some(x, x+1L)) (1L)
   |> Seq.find(fun x-> x%3L=0L && x%7L=0L && x%11L=0L && x%12L=0L && x%13L=0L && x%14L=0L && x%15L=0L && x%16L=0L && x%17L=0L && x%18L=0L && x%19L=0L && x%20L =0L)

printfn "answer = %d" answer