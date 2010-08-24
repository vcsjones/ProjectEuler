let fib = Seq.unfold(fun (p, c) -> Some((p, c), (c, p+c))) (1I,1I)

let answer = 
   (
   fib
   |> Seq.map(fun x -> fst x)
   |> Seq.map(fun x -> x.ToString("R"))
   |> Seq.takeWhile(fun x -> x.Length < 1000)
   |> Seq.toArray
   |> Array.length
   ) + 1

printfn "answer = %d" answer