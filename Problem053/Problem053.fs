let fact n =
   match n with
   | n when n = 0I -> 1I
   | _ -> [1I .. n] |> Seq.reduce(fun acc n -> acc*n)

let c n r = 
   (fact n) / (fact r * fact(n - r))

let answer = 
   seq {
      for n in [1I..100I] do
         for r in [1I..n] do
            yield c n r
   }
   |> Seq.filter(fun x -> x > 1000000I)
   |> Seq.length

printfn "answer = %d" answer