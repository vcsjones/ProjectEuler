let isPandigital n = 
   let str = new string(n.ToString().ToCharArray() |> Array.sort)
   "123456789" = str

let countFactors (n:int) = 
      let rec loop (s:string, f:int) =
         let current = s + string(f*n)
         match current.Length with
         | l when l >= 9 -> if isPandigital current then Some(int current) else None
         | _ -> loop(current, f+1)
      loop("", 1)

let answer = 
   [2..10000]
   |> List.map countFactors
   |> Seq.choose(fun x -> x)
   |> Seq.max

printfn "answer = %d" answer