let sieve limit =
   let mults p = p :: [ (p*p) .. p .. limit ] |> Set.ofList
   let prune y = Set.minElement y, Set.minElement y |> mults |> Set.difference y
   let rec loop (p, pset) ans =
      match pset with
      | x when x = Set.empty -> Set.add p ans
      | _ -> loop (prune pset) (Set.add p ans)
   loop ([ 2L .. limit ] |> Set.ofList |> prune) Set.empty |> Set.toList


let rec gcd n1 n2 = 
   match n2 with
   | 0 -> n1
   | _ -> gcd n2 (n1%n2)

let totient n = 
   [1..n]
   |> List.filter(fun x -> gcd x n = 1)
   |> List.length

let multOrder a m =
   if (gcd a m) <> 1 then failwith "Arguments are not coprime"
   else
      0

printfn "test = %d" (totient 2)