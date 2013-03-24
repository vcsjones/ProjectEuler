let nums = seq {
    for d in [10.0..99.0] do
        for n in [10.0..(d-1.0)] do
            yield (n, d)
}

let simplify (n, d) =
    let rec gcd n d = if d = 0.0 then n else gcd d (n % d)
    let gcf = gcd n d
    (n / gcf, d / gcf)

let cancels (n:float, d:float) =
    let map (s:float) = s.ToString().ToCharArray() |> Array.map(fun x -> float(int(x) - 0x30))
    let ns, ds = map(n), map(d)
    match (ns, ds) with
    | (ns, ds) when ns.[0] = ds.[1] -> ns.[1] / ds.[0] = n / d
    | (ns, ds) when ns.[1] = ds.[0] -> ns.[0] / ds.[1] = n / d
    | _ -> false

let answer =
    nums
    |> Seq.filter cancels
    |> Seq.reduce(fun (accn, accd) (n, d) -> (accn*n, accd*d))
    |> simplify
    |> snd

printfn "%A" answer