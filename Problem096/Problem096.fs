open System.IO
open System.Text.RegularExpressions

let problems =
    Regex.Split(File.ReadAllText("sudoku.txt"), @"^Grid \d\d", RegexOptions.Multiline)
    |> Array.filter(fun x -> x.Trim().Length > 0)
    |> Array.map(fun x -> 
                        let y = Regex.Split(x.Trim(), @"\r\n")
                                |> Array.map(fun x -> x.ToCharArray() |> Array.map(fun x -> int(x) - 0x30))
                        array2D(y))

assert (problems |> Array.length = 50)

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

//Fill in all of the places that require no guessing. This can also be used
//On complex problems to reduce the complexity for spaces that have known answers
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

//Use a guess and check method. Iterate over all possible answers
let complexSolve (a:int[,]) =
    let a = a |> Array2D.copy
    let w = (a |> Array2D.length1)-1
    let h = (a |> Array2D.length2)-1
    a


let solve (a:int[,]) =
    a |> simpleSolve |> complexSolve

let isSolved (a:int[,]) = 
    a |> flatten |> Array.forall(fun x -> x <> 0)

let answers = 
    problems
    |> Array.map solve
    |> Array.filter(fun x -> not (isSolved(x)))

printfn "%A" (answers)