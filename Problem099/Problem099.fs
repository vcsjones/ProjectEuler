open System.IO


let parseLine (str:string[]) = 
    match str with
    | [|a; b|] ->  (a |> float, b |> float)
    | _ -> failwith("Unexpected line")

let answer = 
    File.ReadLines("base_exp.txt")
    |> Seq.mapi(fun i x -> (i, x.Split(',') |> parseLine))
    |> Seq.map(fun (i, (n, e)) -> (i, e *  log n))
    |> Seq.maxBy snd
    |> fst

printfn "answer = %i" answer