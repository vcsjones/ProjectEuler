let sb = new System.Text.StringBuilder()

let superString = 
   [1..1000000] |> Seq.fold(fun acc x -> sb.Append(x)) sb
   |> string

let d n = int(superString.[n-1]) - 0x30

let answer = d(1) * d(10) * d(100) * d(1000) * d(10000) * d(100000)

printfn "answer = %d" answer