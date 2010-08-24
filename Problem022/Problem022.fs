let score name index = 
      (
         name
         |> Seq.map(fun c -> int(c)-64)
         |> Seq.sum
      ) *index

let answer = System.IO.File.ReadAllText(".\\names.txt").Split(',')
               |> Array.map(fun x -> x.Trim('"'))
               |> Array.sort
               |> Array.mapi(fun i x -> score x (i+1))
               |> Array.sum

printfn "answer = %d" answer