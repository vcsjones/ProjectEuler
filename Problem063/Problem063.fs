let len n =
   n.ToString().Length

let pow n p = 
   let rec loop nl c =
      if c = p then nl
      else nl * (loop nl (c+1L))
   loop n 1L

let answer =
   seq {
      for x in [8I .. 8I] -> seq { for y in [2..50] do if len(x ** y) = y then yield y }
   }
   //|> Seq.take 25
   |> Seq.iter(fun x -> printfn "pow = %A" x)