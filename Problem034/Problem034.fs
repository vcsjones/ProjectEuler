let factorial n =
   match n with
   | 0 -> 1
   | _ -> [1 .. n] |> Seq.reduce(fun acc n -> acc*n)


let sumFactorDigits n = 
    n.ToString()
    |> Seq.map(fun x -> factorial(int x - 0x30))
    |> Seq.sum

let answer = 
    [3..100000]
    |> Seq.map(fun x -> (x, sumFactorDigits x))
    |> Seq.filter(fun (x, y) -> x=y)
    |> Seq.sumBy fst

printfn "answer = %d" answer