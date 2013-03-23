let generateWithLength (generator : int -> int) =
    Seq.initInfinite(fun x -> x+1)
    |> Seq.map(fun x -> (x, generator x))
    |> Seq.skipWhile(fun (_, g) -> g.ToString().Length < 4)
    |> Seq.takeWhile(fun (_, g) -> g.ToString().Length = 4)
    |> Seq.toList

let triangles = generateWithLength (fun n -> n*(n+1)/2)
let squares = generateWithLength (fun n -> n*n)
let pentagonals = generateWithLength (fun n -> n*(3*n-1)/2)
let hexagonals = generateWithLength (fun n -> n*(2*n-1))
let heptagonals = generateWithLength (fun n -> n*(5*n-3)/2)
let octagonals = generateWithLength (fun n -> n*(3*n-2))

let matchEnds (back : int) (front : int) = 
    let back, front = back.ToString(), front.ToString()
    front.Substring(0, 2) = back.Substring(back.Length-2, 2)

let collection = [triangles; squares; pentagonals; hexagonals; heptagonals; octagonals];

let answer = 
    let rec iter (items: (int * int) list list) (nums : int list list) : int list =
        match items with
        | head :: tail -> 
            let remainder = List.concat(tail)
            nums
            []
        | [] -> []
    let chain = iter collection [triangles |> List.map fst] //|> List.sumBy snd
    chain

printfn "%A" answer
