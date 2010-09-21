let factorial n = [2I .. n-1I] |> Seq.fold(fun acc n -> acc*n) 1I
let answer = 
   (factorial 100I).ToString("R").ToCharArray()
   |> Seq.map(fun x -> int x - 0x30)
   |> Seq.sum

printfn "answer = %d" answer