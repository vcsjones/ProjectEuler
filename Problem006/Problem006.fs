let sumOfSquares = seq {for x in 1..100 -> x*x} |> Seq.sum
let squareOfSums = (([1..100] |> Seq.sum |> float) ** 2.0) |> int
let answer = squareOfSums - sumOfSquares

printfn "answer = %d" answer 