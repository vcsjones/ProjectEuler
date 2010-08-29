open System.IO

let t n = 0.5*n*(n+1.0)

let wordValue (str:string) = 
   str.ToCharArray()
   |> Seq.map(fun x -> int(x) - 64)
   |> Seq.sum
   |> float

let isTriangleNumber n = 
   [for x in [1.0..n] -> t x]
   |> List.exists(fun x -> x = n)

let answer = 
   File.ReadLines(".\\words.txt")
   |> Seq.filter(fun x -> isTriangleNumber (wordValue x))
   |> Seq.toArray
   |> Seq.length

printfn "answer = %d" answer
