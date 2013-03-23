let nums = seq {
    for n in [10.0..99.0] do
        for d in [10.0..99.0] do
            if d > n then
                yield (n, d)
}

let reduce (n, d) =
    let rec gcd n d =
        match d with
        | d when d > 0.0 -> gcd d (n % d)
        | _ -> n
    let gcf = gcd n d
    (n / gcf, d / gcf)

let cancels (n, d) =
    let ns, ds = n.ToString(), d.ToString()
    if ns.[0] = ds.[1] && float(int(ns.[1]) - 0x30) / float(int(ds.[0]) - 0x30) = n / d then
        true
    else if ns.[1] = ds.[0] && float(int(ns.[0]) - 0x30) / float(int(ds.[1]) - 0x30) = n / d then
        true
    else
        false

let answer =
    nums
    |> Seq.filter cancels
    |> Seq.reduce(fun (accn, accd) (n, d) -> (accn*n, accd*d))
    |> reduce
    |> snd

printfn "%A" answer