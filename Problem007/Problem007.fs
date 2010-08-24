let answer = 
   Seq.initInfinite(fun x -> x+1)
   |> Seq.filter(fun x -> [2..x|> float |> sqrt |> int] |> Seq.forall(fun y -> y=x || x%y<>0))
   |> Seq.nth 10001

printfn "answer = %d" answer