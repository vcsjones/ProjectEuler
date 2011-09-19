let isPrime n =
    if n = 1L then false
    else [|2L.. (n|>float|>sqrt|>int64)|]
         |> Array.exists(fun y -> n <> y && n % y=0L)
         |> not

let diagonals = 
    Seq.unfold(fun (n, c, s, w, t, p) ->
                                let prime = if isPrime(n+s) then p+1.0f else p

                                if c = 4L then Some((n, c, s, w, t, p), (n+s, 1L, s+2L, w, t+1.0f, prime))
                                else Some((n, c, s, w, t, p), (n+s, c+1L, s, (if c = 1L then w+2L else w), t+1.0f, prime))
                ) (1L, 1L, 2L, 1L, 1.0f, 0.0f)
let answer = 
    diagonals
    |> Seq.map(fun (n, c, s, w, t, p) -> (n, c, s, w, t, p, p/t))
    |> Seq.skipWhile(fun (n, c, s, w, t, p, a) -> n = 1L || a > 0.1f)
    |> Seq.map(fun (_, _, _, w, _, _, _) -> w)
    |> Seq.head

printfn "%i" answer