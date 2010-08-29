let sieve limit =
   let mults p = p :: [ (p*p) .. p .. limit ] |> Set.ofList
   let prune y = Set.minElement y, Set.minElement y |> mults |> Set.difference y
   let rec loop (p, pset) ans =
      match pset with
      | x when x = Set.empty -> Set.add p ans
      | _ -> loop (prune pset) (Set.add p ans)
   loop ([ 3L..2L..limit ] |> Set.ofList |> prune) Set.empty |> Set.toList

let rotate arr = 
   seq {
      let len = Array.length arr
      for x in [0..len-1] do
      yield arr|>Array.permute(fun i -> (i + x) % len)
   }

let isPrime n = [|2L.. (n|>float|>sqrt|>int64)|] |> Array.exists(fun y -> n <> y && n % y=0L) |> not

let answer = 
   seq {yield 2L; yield! sieve 1000000L}
   |> Seq.map(fun x -> (x, x.ToString().ToCharArray() |> rotate |> Seq.map(fun x -> int64(new string(x)))))
   |> Seq.filter(fun (x, a) -> a |> Seq.forall isPrime)
   |> Seq.toArray
   |> Array.length

printfn "answer = %d" answer