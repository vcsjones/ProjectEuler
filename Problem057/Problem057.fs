let generate =
    Seq.unfold (fun (n, d) -> Some((n, d), (d*2I+n, d+n))) (3I, 2I)

let answer = 
    generate
    |> Seq.take 1000
    |> Seq.filter(fun (n, d) -> n.ToString("R").Length > d.ToString("R").Length)
    |> Seq.length

printfn "%A" answer