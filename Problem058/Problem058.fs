let isPrime n =
    if n = 1L then false
    else [|2L.. (n|>float|>sqrt|>int64)|]
         |> Array.exists(fun y -> n <> y && n % y=0L)
         |> not


let answer = 
    let rec wrap (n, c, s, w, t, p) =
        match (p/t) with
        | a when a <= 0.1f && n > 1L -> w
        | _ ->
                let prime = if isPrime(n+s) then p+1.0f else p                 
                match c with
                | 4L -> wrap(n+s, 1L, s+2L, w, t+1.0f, prime)
                | 1L -> wrap(n+s, c+1L, s, w+2L, t+1.0f, prime)
                | _  -> wrap(n+s, c+1L, s, w, t+1.0f, prime)
    wrap(1L, 1L, 2L, 1L, 1.0f, 0.0f)


printfn "answer = %i" answer