let numbers = [0;1;2;3;4;5;6;7;8;9]

let rec permutations list taken =
    seq {
        if Set.count taken = List.length list then yield [] else
            for l in list do
                if not (Set.contains l taken) then
                    for perm in permutations list (Set.add l taken)  do
                        yield l::perm }

let p = permutations numbers Set.empty
let answer = p |> Seq.skip 999999 |> Seq.head
printfn "%A" answer