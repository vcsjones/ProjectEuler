type FractionValue =
    {
        n : int
        d : int
    }
    member this.Reduce() =
        let rec gcd n d = if d = 0 then n else gcd d (n % d)
        let g = gcd this.n this.d
        {n = this.n / g; d = this.d / g;}


let ns = Seq.initInfinite(fun x -> x+1) |> Seq.take 10

let iters count = 
    seq {
        for n in [1..count] do
            for x in [1..n] do
                let y = n-x
                if y <> 0 then
                    yield (n, x, (n-x))
    }

iters 10 |> Seq.iter(fun t ->
        printfn "%A" t
        System.Console.ReadKey(true) |> ignore
    )