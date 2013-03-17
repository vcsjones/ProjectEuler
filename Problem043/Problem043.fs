let pandigitalNumbers = 
  let rec permutations list taken = 
      seq { if Set.count taken = Array.length list then yield [] else
            for l in list do
              if not (Set.contains l taken) then 
                for perm in permutations list (Set.add l taken)  do
                  yield l::perm }
  permutations ("0123456789".ToCharArray()) Set.empty 


let testNumber (str : char list) =
    let primes = [|2;3;5;7;11;13;17|];
    str
    |> Seq.skip 1
    |> Seq.windowed 3
    |> Seq.mapi(fun i x -> (i, x))
    |> Seq.forall(fun (i, x) -> (new string(x) |> int) % primes.[i] = 0)

let answer = 
    pandigitalNumbers
    |> Seq.filter(testNumber)
    |> Seq.map(fun x -> bigint.Parse(new string(x |> List.toArray)))
    |> Seq.sum


printfn "%A" answer