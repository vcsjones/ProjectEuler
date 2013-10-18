open System

let rec calculate (n : double) result lowerBound = 
    match lowerBound with
    | l when l >= 10 -> result
    | _ ->
        let newLowerBound = Math.Pow(10.0, (n-1.0)/n) |> ceil |> int
        calculate (n+1.0) (result + 10 - newLowerBound) newLowerBound
    
let answer = calculate 1.0 0 0
printfn "%i" answer