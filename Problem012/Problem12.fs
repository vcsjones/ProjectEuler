let triangleNumbers = Seq.unfold(fun (p, c) -> Some((p, c), (p+1, c+p))) (2, 1) |> Seq.map(fun (p, c) -> c)
let countDivisors x = [1..x|>float|>sqrt|>int]
                      |> Seq.filter(fun y -> x%y=0)
                      |> Seq.collect(fun y -> [y;x/y])
                      |> Seq.length

let answer = 
   triangleNumbers
   |> Seq.filter(fun x -> (countDivisors x) > 500)
   |> Seq.head

printfn "answer = %d" answer