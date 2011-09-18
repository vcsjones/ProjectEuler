let max = System.UInt64.MaxValue;

let answer =
    1UL
    |> Seq.unfold(fun x -> Some(x, x+1UL))
    |> Seq.map(fun x -> (x, x.ToString("X")))
    |> Seq.takeWhile(fun (_, x) -> x.Length <= 16)
    |> Seq.filter(fun (_, x) -> x.Contains("A") && x.Contains("0") && x.Contains("F"))
    |> Seq.sumBy(fst)

printfn "answer = %x" answer