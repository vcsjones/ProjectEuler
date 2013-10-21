open System.Threading.Tasks
open Common.Concurrency

let rec gcd n d = if d = 0 then n else gcd d (n % d)

let limit = 12000
let fractions =
    seq {
        for d in [1..limit] do
            for n in [1..(d-1)] do
                yield (n, d)
    }


let body (n:int, d:int) (state:ParallelLoopState) (result:int) =
    if gcd n d <> 1 then result
    else
        let value = float n / float d
        if value > 1.0/3.0 && value < 1.0/2.0 then result+1 else result


let counter = ref 0
Parallel.ForEach(fractions, (fun () -> 0), body, (fun n -> interlockAdd counter n |> ignore)) |> ignore
printfn "answer = %A" !counter