let isPandigital n = 
   let str = new string(n.ToString().ToCharArray() |> Array.sort)
   "123456789" = str

let answer =
    seq {
        for x in [2L..100L] do
            let s =  if x > 9L then 123L else 1234L
            for y in [s..(10000L/(x+1L))] do
                let str = System.String.Concat(x, y, x*y) |> int64
                if isPandigital str then yield x*y
    }
    |> Seq.distinct
    |> Seq.sum

printfn "%i" answer