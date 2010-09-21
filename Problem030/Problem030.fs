let fifthSum (n:float) = 
   Seq.map(fun x -> float(int(x)-0x30)**5.0) (string n)
   |> Seq.sum

let answer = 
   [2.0 .. 9.0**5.0*6.0]
   |> Seq.map(fun x -> (x, fifthSum x))
   |> Seq.filter(fun (x, y) -> x=y)
   |> Seq.sumBy fst

printfn "answer = %f" answer