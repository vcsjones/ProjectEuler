let factorial n = [1 .. n] |> Seq.fold(fun acc n -> acc*n) 1


let sumFactorDigits n = 
    n.ToString().ToCharArray()
    |> Seq.map(fun x -> factorial(int(x) - 0x30))
    |> Seq.sum

let answer = 
    [3..100000]
    |> Seq.map(fun x -> (x, sumFactorDigits x))
    |> Seq.filter(fun (x, y) -> x=y)
    |> Seq.iter(fun (x, _) -> printfn "%d" x)