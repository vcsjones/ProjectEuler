let sortDigits n =
   let str = n.ToString()
   new string(str.ToCharArray() |> Array.sort)

let areAllSame (n1, n2, n3, n4, n5) =
   n1 = n2 && n1 = n3 && n1 = n4 && n1 = n5

let answer = 
   Seq.initInfinite(fun x -> x+1)
   |> Seq.filter(fun x -> areAllSame (sortDigits(x*2), sortDigits(x*3), sortDigits(x*4), sortDigits(x*5), sortDigits(x*6)))
   |> Seq.head

printfn "answer = %d" answer