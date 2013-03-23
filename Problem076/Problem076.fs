let rec count sum numbers =
    if sum = 0 then [[]]
    else 
        match numbers with
        | head::tail -> 
            let xs = count sum tail
            if sum >= head then
                let ys = count (sum - head) numbers |> List.map (fun xs -> head :: xs)
                List.append xs ys
            else
                xs
        | [] -> []

let answer = 
    count 100 [100..-1..1]
    |> List.length

printfn "%A" answer