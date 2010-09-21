let hugeSum =
   [1..1000]
   |> Seq.sumBy(fun x -> bigint x**x)
   |> string

let answer = hugeSum.[(hugeSum.Length - 10)..]
printfn "answer = %s" answer