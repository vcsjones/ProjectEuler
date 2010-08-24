let revString (str:string) =
   new string(str.ToCharArray() |> Array.rev)

let answer =
   seq {
      for x in [100..999] do
         for y in [100..999] do
            yield x*y
   }
   |> Seq.map(fun x -> (x, x.ToString()))
   |> Seq.filter(fun (x,s) -> s = (revString s))
   |> Seq.maxBy fst
   |> fst

printfn "answer = %d" answer