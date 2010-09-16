let hugeSum =
   ([1I..1000I]
   |> Seq.fold(fun acc x -> acc + (x**int(x))) (0I)
   ).ToString("R")

let answer = hugeSum.[(hugeSum.Length - 10)..]
printfn "answer = %s" answer