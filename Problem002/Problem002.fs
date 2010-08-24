 let answer = 
   Seq.unfold(fun (p, c) -> Some((p, c), (c, p+c))) (1,2)
   |>Seq.map(fun (p, c) -> c)
   |>Seq.takeWhile(fun x -> x<4000000)
   |>Seq.filter(fun x -> x%2=0)
   |>Seq.sum
   
printfn "%d" answer