open System.IO

let t n = 0.5*n*(n+1.0)

let wordValue (str:string) = 
   str.ToCharArray()
   |> Seq.map(fun x -> int(x) - 64)
   |> Seq.sum
   |> float

let isTriangleNumber n = 
   seq {for x in [1.0..n] do if (t x) = n then yield n}
   |> Seq.length > 0

let answer = 
   File.ReadLines(".\\words.txt")
   |> Seq.map wordValue
   |> Seq.filter isTriangleNumber
   |> Seq.length

printfn "answer = %d" answer
