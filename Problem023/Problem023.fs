let divisors x =
   [1..x|>float|>sqrt|>int]
   |> Seq.filter(fun y -> x%y=0)
   |> Seq.map(fun y -> [y;x/y])
   |> Seq.concat
   |> Seq.filter(fun y -> x<>y)
   |> Seq.sum


let abundantNumbers =
   [1..28123]
   |> Seq.filter(fun x -> (divisors x) > x)
   