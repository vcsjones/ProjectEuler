let pandigitalNumbers = 
  let rec permutations list taken = 
      seq { if Set.count taken = Array.length list then yield [] else
            for l in list do
              if not (Set.contains l taken) then 
                for perm in permutations list (Set.add l taken)  do
                  yield l::perm }
  permutations ("0123456789".ToCharArray()) Set.empty 
  |> Seq.map(fun f -> new string(f |> List.toArray) |> int64)


let answer = 
    pandigitalNumbers
    |> Seq.sort
    |> Seq.nth 1000000

printfn "%i" answer