let arr =
    array2D
        [
            [0; 0; 3; 0; 2; 0; 6; 0; 0;];
            [9; 0; 0; 3; 0; 5; 0; 0; 1;];
            [0; 0; 1; 8; 0; 6; 4; 0; 0;];
            [0; 0; 8; 1; 0; 2; 9; 0; 0;];
            [7; 0; 0; 0; 0; 0; 0; 0; 8;];
            [0; 0; 6; 7; 0; 8; 2; 0; 0;];
            [0; 0; 2; 6; 0; 9; 5; 0; 0;];
            [8; 0; 0; 2; 0; 3; 0; 0; 9;];
            [0; 0; 5; 0; 1; 0; 3; 0; 0;]
        ]

let numbers = [|1..9|] |> Set.ofArray

let flatten (a:'T[,]) =
    seq { for y in a -> y :?> 'T }
    |> Seq.toArray

let quadrant (a:int[,]) (x:int, y:int) =
    let inline n nz = (nz / 3) * 3
    Array2D.init 3 3 (fun i j -> a.[n y + i, n x + j])

let axes (a:int[,]) (x:int, y:int) =
    let xa = Array2D.init 1 9 (fun i j -> a.[y, j]) |> flatten
    let ya = Array2D.init 9 1 (fun i j -> a.[i, x]) |> flatten
    (xa, ya)

let solutions (a:int[,]) (x:int, y:int) = 
    let quad = quadrant a (x,y)
    let quadExclude = flatten quad |> Array.filter(fun x -> x <> 0)
    let axesExclude = 
        let a = axes a (x, y)
        (fst a) |> Array.append (snd a)
    let excludes = quadExclude |> Array.append axesExclude |> Set.ofArray
    numbers - excludes |> Set.toArray

let simpleSolve (a:int[,]) =
    let a = a |> Array2D.copy
    let w = (a |> Array2D.length1)-1
    let h = (a |> Array2D.length2)-1
    let rec doSolve (a:int[,]) = 
        let mutable affected = 0
        for x in [0.. w] do
            for y in [0.. h] do
                if a.[y,x] = 0 then
                    let sol = solutions a (x, y)
                    if sol |> Array.length = 1 then
                        a.[y, x] <- sol.[0]
                        affected <- affected + 1
        if affected = 0 then a else doSolve a
    doSolve a

let solve (a:int[,]) =
    let simple = simpleSolve a
    simple

let answer = solve arr

printfn "%A" answer