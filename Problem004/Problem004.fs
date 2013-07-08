let revString (str:string) =
   new string(str.ToCharArray() |> Array.rev)

let answer =
   [100..999]
   |> Seq.map(fun m -> ( Seq.init (m |> int) (fun _ -> m), [1..m]))
   |> Seq.collect(fun (l1, l2) -> Seq.zip l1 l2 |> Seq.map(fun (x, y) -> x*y))
   |> Seq.map(fun x -> (x, x.ToString()))
   |> Seq.filter(fun (x,s) -> s = (revString s))
   |> Seq.maxBy fst
   |> fst

printfn "answer = %d" answer