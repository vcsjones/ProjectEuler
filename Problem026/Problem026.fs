let rec quotient n d =
    seq {
        yield (n, d)
        yield! quotient ((n%d)*10) d
    }

let repeater n d =
    let rec counter (items:(int * int) list) (push:List<_>) = 
        match items with
        | head :: tail when push |> List.exists(fun x -> x = head) ->  [head] |> List.append push
        | head :: tail -> counter tail ([head] |> List.append push)
        | [] -> failwith "Non-repeating result encountered."
    counter ((quotient n d) |> Seq.take 2000 |> Seq.toList) (List.empty)

let resolveQ (s:List<(int * int)>) =
    let last = s |> Seq.last
    let chop = s |> List.findIndex(fun x -> x = last)
    s |> Seq.skip chop |> Seq.take (s.Length - chop-1) |> Seq.map(fun (n, d) -> n/d) |> Seq.toList

let answer = 
    [1..1000]
    |> Seq.map(fun x ->(x, (repeater 1 x) |> resolveQ))
    |> Seq.sortBy (fun (n, x) -> x |> List.length)
    |> Seq.last
    |> fst

printfn "%A" answer