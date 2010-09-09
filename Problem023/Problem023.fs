let divisors x =
   [1..x|>float|>sqrt|>int]
   |> Seq.filter(fun y -> x%y=0)
   |> Seq.map(fun y -> [y;x/y])
   |> Seq.concat
   |> Seq.filter(fun y -> x<>y)
   |> Seq.distinct
   |> Seq.sum


let abundantNumbers =
   [1..28123]
   |> Seq.filter(fun x -> (divisors x) > x)
   |> Seq.toList

let hasSum n = 
   abundantNumbers
   |> List.exists(fun x -> abundantNumbers |> List.exists(fun y -> n-x=y))

let answer = 
   [1..28123]
   |> Seq.filter(fun x -> not(hasSum x))
   |> Seq.sum

printfn "answer = %d" answer