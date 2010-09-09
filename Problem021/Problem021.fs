let d n = ([1..n|>float|>sqrt|>int]
             |> Seq.filter(fun x -> n%x=0)
             |> Seq.map(fun x -> x+n/x)
             |> Seq.sum) - n

let answer = 
   [1..9999]
   |> Seq.map(fun x -> (x, d x))
   |> Seq.filter(fun (x, y) -> x <> y && d y = x)
   |> Seq.sumBy fst

printfn "answer = %d" answer