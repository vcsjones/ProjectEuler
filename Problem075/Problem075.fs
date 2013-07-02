type Triangle =
    {
        a: int64
        b: int64
        c: int64
    }
    member this.Perimeter =
            this.a + this.b + this.c
    static member (+) (left : Triangle, right : Triangle) =
        {a=left.a + right.a; b=left.b+right.b; c = left.c + right.c}
            

let rec gcd n d = if d = 0L then n else gcd d (n % d)

let limit = 1500000L

let answer = 
    [2L..System.Math.Sqrt(float(limit/2L)) |> int64]
    |> Seq.map(fun m -> ( Seq.init (m |> int) (fun _ -> m), [1L..m]))
    |> Seq.collect(fun (l1, l2) -> Seq.zip l1 l2)
    |> Seq.filter(fun (m, n) -> (n+m)%2L=1L && (gcd n m) = 1L)
    |> Seq.map(fun (m, n) -> {a = m*m+n*n;b = m*m-n*n;c=2L*m*n;})
    |> Seq.collect(fun t -> t |> Seq.unfold(fun t2 -> Some(t2, t2 + t)) |> Seq.takeWhile(fun t -> t.Perimeter <= limit))
    |> Seq.groupBy(fun t -> t.Perimeter)
    |> Seq.filter(fun (_, t) -> t |> Seq.length = 1)
    |> Seq.map(fun (_, t) -> t |> Seq.exactlyOne)

printfn "%A" (answer |> Seq.length)