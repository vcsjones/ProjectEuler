open System.IO
open System.Text.RegularExpressions

let tm apply t =
    (t |> fst |> apply, t |> snd |> apply)

module Array =
    let last (a:'T[]) = a.[(a |> Array.length) - 1]

let problems =
    Regex.Split(File.ReadAllText("sudoku.txt"), @"^Grid \d\d", RegexOptions.Multiline)
    |> Array.filter(fun x -> x.Trim().Length > 0)
    |> Array.map(fun x -> 
                        let y = Regex.Split(x.Trim(), @"\r\n")
                                |> Array.map(fun x -> x.ToCharArray() |> Array.map(fun x -> int(x) - 0x30))
                        array2D(y))

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

let getZeros (a:int[,]) =
    seq {
    let w = (a |> Array2D.length1)-1
    let h = (a |> Array2D.length2)-1
    for x in [0.. w] do
            for y in [0.. h] do
                if a.[y,x] = 0 then yield (x,y)
    }


//Verify it is solved correctly
//Has overhead. Review candidate.
let isSolved (a:int[,]) = 
    let w = (a |> Array2D.length1) - 1
    let h = (a |> Array2D.length2) - 1
    let mutable pass = true
    for x in [0..w] do
        for y in [0..h] do
            let coordinate = (x, y)
            let quad = quadrant a coordinate |> flatten |> Set.ofArray = numbers
            let ax = axes a coordinate |> tm (fun x -> x |> Set.ofArray = numbers)
            pass <- pass && quad && fst ax && snd ax
    pass

let incrementWorkState(ws:(int * array<int>)[]) = 
    let rec inc (i:int) = 
        let item = ws.[i]
        let current = fst item
        let possibles = snd item
        let last = possibles |> Array.last
        if current <> last then
            let next = possibles.[(possibles |> Array.findIndex(fun x -> x = current))+1]
            let newItem = (next, possibles)
            printfn "%A" newItem
            ws.[i] <- newItem
    inc 0
        

let complexSolve (a:int[,]) = 
    let a = a |> Array2D.copy
    let zeros = getZeros a
                |> Seq.map(fun x -> (x, solutions a x)) 
                |> Seq.toArray
                |> Array.sortBy(fun (_, x) -> x |> Array.length)
    let workState = zeros |> Array.map(fun ((x, y), a) -> (a.[0], a))
    incrementWorkState(workState)
    
let answers = 
    problems
    |> Seq.skip 1
    |> Seq.map complexSolve
    |> Seq.head

printfn "%A" (answers)