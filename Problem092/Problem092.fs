open System
open System.Threading
open System.Threading.Tasks

let sumDigitSquares n =
      n.ToString().ToCharArray()
      |> Array.map(fun x -> (float x - 48.0)**2.0)
      |> Array.sum
      |> int

let findDigitSquare n = 
   let rec loop l = 
      match l with
      | 1 -> false
      | 89 -> true
      | x -> loop(sumDigitSquares(x))
   loop n

let actImpl n pls loc = 
   if findDigitSquare n then loc + 1
   else loc

let counter = ref 0

Parallel.For(1, 10000000, (fun () -> 0), actImpl, (fun n -> Interlocked.Add(counter, n) |> ignore)) |> ignore

printfn "answer = %d" !counter