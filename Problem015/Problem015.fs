let seq row
      = Seq.unfold (fun (entry, col) -> Some((entry, col), ((entry * (row + 1L - col)) / col, col + 1L))) (1L,1L)

let pascalTriangle = 
     seq (20L*2L)
     |> Seq.skipWhile(fun (entry,col) -> col <= 20L)
     |> Seq.head
     |> fst

printfn "answer = %d" pascalTriangle