open System
open System.Threading
open System.Threading.Tasks

let reverseNumber n = 
   new string(n.ToString().ToCharArray() |> Array.rev)

let allDigitsOdd n = 
   n.ToString().ToCharArray()
   |> Seq.map(fun x -> int(x) - 0x30)
   |> Seq.forall(fun x -> x%2<>0)

let initImpl() = 0
let init = new Func<int>(initImpl)

let actImpl n pls loc = 
      if n%10 = 0 then loc
      else
         let reverse = reverseNumber n
         let sum = n + int(reverseNumber n)
         if sum%2<>0 && reverse.[0] <> '0' && allDigitsOdd sum then
            loc + 1
         else
            loc

let act = new Func<int, ParallelLoopState, int, int>(actImpl)

let mutable counter = 0

let finImpl n = 
   Interlocked.Add(&counter, n) |> ignore

let fin = new Action<int>(finImpl)

Parallel.For(1, 1000000000, init, act, fin) |> ignore

printfn "answer = %d" counter