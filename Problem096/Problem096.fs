open Common
open Common.Timer
open System.IO
open System.Text
open System.Text.RegularExpressions

let flatten (a:'T[,]) =
    seq {
        for d1 in [0.. (a |> Array2D.length1)-1] do
            for d2 in [0 .. (a |> Array2D.length2)-1] do
                yield a.[d1, d2]
    }

type Puzzle(data : int option[,]) = 

    static let width = 9
    static let height = 9
    static let cellsize = 3
    static let n nz = (nz / cellsize) * cellsize
    
    let superState : (int[] * int * int) [,] =
        let valids = [1..9] |> Set.ofList
        Array2D.init 9 9 (fun x y ->
                match data.[x, y] with
                | Some _ ->
                    (Array.empty, x ,y)
                | None ->
                    let xAxis = Array2D.init 1 width (fun _ j -> data.[j, y]) |> flatten
                    let yAxis = Array2D.init 1 height (fun _ j -> data.[x, j]) |> flatten
                    let cell = Array2D.init cellsize cellsize (fun i j -> data.[n x + i, n y + j]) |> flatten
                    let known = seq { yield! xAxis; yield! yAxis; yield! cell} |> Seq.choose id |> Set.ofSeq
                    (valids - known |> Set.toArray, x, y)
            )

    static member Zero = 
        Puzzle(Array2D.init width height (fun _ _ -> None))

    member private this.Contents = data

    member this.State = superState

    member this.Minimize() = 
        duration "Minimize" (fun () ->
            if superState |> flatten |> Seq.forall(fun (arr, _, _) -> arr |> Array.length <> 1) then this 
            else
                let rec simplify (p : Puzzle) =
                    let fixup = p.State |> flatten |> Seq.tryFind(fun (arr, y, x) -> arr |> Array.length = 1)
                    match fixup with
                    | Some (arr, y, x) -> simplify (p.Set (x, y) (Some arr.[0]))
                    | None -> p
                simplify this
        )

    member this.Set (x, y) value =
        let copy = Array2D.copy data
        copy.[y, x] <- value
        Puzzle(copy)

    member this.AxisX row =
        Array2D.init 1 width (fun _ j -> data.[row, j]) |> flatten

    member this.AxisY column =
        Array2D.init 1 height (fun i _ -> data.[i, column]) |> flatten

    member this.QuadrantForCell x y =
        Array2D.init cellsize cellsize (fun i j -> data.[n y + i, n x + j])

    member this.Item
        with get (x, y) = data.[y, x]

    member this.IsSolved = 
        data |> flatten |> Seq.forall Option.isSome

    override this.ToString() =
        let builder = new StringBuilder()
        for x in [0..8] do
            if x<> 0 then builder.AppendLine() |> ignore
            if x%3=0 then builder.AppendLine(new System.String('─', 11)) |> ignore
            for y in [0..8] do
                if (y%3=0 && y > 0) then builder.Append("│") |> ignore
                builder.Append(match this.[x, y] with | Some x -> x.ToString() | None -> " ") |> ignore
        builder.ToString()

    interface System.IEquatable<Puzzle> with
        member this.Equals(other : Puzzle) = 
            flatten this.Contents |> Seq.equal (flatten other.Contents)

let problems =
    Regex.Split(File.ReadAllText("sudoku.txt"), @"^Grid \d\d", RegexOptions.Multiline)
    |> Array.filter(fun x -> x.Trim().Length > 0)
    |> Array.map(fun x -> 
                        let y = Regex.Split(x.Trim(), @"\r\n") |> Array.map(fun x -> x.ToCharArray() |> Array.map(fun x -> int(x) - 0x30) |> Array.map(fun x -> if x=0 then None else Some x))
                        Puzzle(array2D(y)))

let solve (p : Puzzle) : Puzzle = 
    let minimized = p.Minimize()
    if minimized.IsSolved then minimized
    else
        let rec scan (p : Puzzle) (x, y) = 
            let state = p.State |> flatten |> List.ofSeq
            match state with
            | head :: tail -> p
            | [] -> p
        scan p (0,0)

