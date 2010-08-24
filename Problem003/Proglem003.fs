let sieve limit =
   let mults p = p :: [ (p*p) .. p .. limit ] |> Set.ofList
   let prune y = Set.minElement y, Set.minElement y |> mults |> Set.difference y
   let rec loop (p, pset) ans =
      match pset with
      | x when x = Set.empty -> Set.add p ans
      | _ -> loop (prune pset) (Set.add p ans)
   loop ([ 2L .. limit ] |> Set.ofList |> prune) Set.empty |> Set.toList

let number = 600851475143L
let answer =
   let root = number |> double |> sqrt |> int64
   sieve root
   |> List.filter(fun x -> number%x = 0L)
   |> List.max

printfn "answer = %d" answer