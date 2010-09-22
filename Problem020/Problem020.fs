let factorial n = [2I .. n-1I] |> Seq.reduce(fun acc n -> acc*n)
let answer = 
   (factorial 100I).ToString("R")
   |> Seq.map(fun x -> int x - 0x30)
   |> Seq.sum

printfn "answer = %d" answer