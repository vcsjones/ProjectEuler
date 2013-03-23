let rec change sum coins =
    if sum = 0 then [[]]
    else 
        match coins with
        | head::tail -> 
            let xs = change sum tail
            if sum >= head then
                let ys = change (sum - head) coins |> List.map (fun xs -> head :: xs)
                List.append xs ys
            else
                xs
        | [] -> []

let answer = 
    change 200 [200;100;50;20;10;5;2;1]
    |> List.length

printfn "%A" answer