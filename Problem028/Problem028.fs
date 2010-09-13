let counter upto = 
   (Seq.unfold(fun (x, i, c) -> Some((x, i, c),(x+i, (if c%4=0 then i+2 else i), c+1))) (3, 2, 2)
   |> Seq.map(fun (x, _, _) -> x)
   |> Seq.take (upto * 2 - 2)
   |> Seq.sum) + 1

let answer = counter 1001

printfn "answer = %d" answer