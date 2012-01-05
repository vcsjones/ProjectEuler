open System.IO

let matrix = array2D (File.ReadAllLines("matrix.txt") 
                      |> Array.map (fun l -> l.Split(',') |> Array.map int32))

let size = matrix |> Array2D.length1

let sum = 
    Array2D.init size size (fun i j -> if i = 0 && j = 0 then matrix.[i, j] else 0)

let top i j = sum.[i - 1, j] + matrix.[i, j]
let left i j = sum.[i, j - 1] + matrix.[i, j]



for i = 0 to size - 1 do
    for j = 0 to size - 1 do
        match (i, j) with
        | (0, 0) -> ()
        | (_, 0) -> sum.[i, j] <- top i j
        | (0, _) -> sum.[i, j] <- left i j
        | (_, _) -> sum.[i, j] <- min (top i j) (left i j)

let answer = sum.[size - 1, size - 1]

printfn "%i" answer