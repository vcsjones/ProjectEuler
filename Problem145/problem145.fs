open System
open System.Threading
open System.Threading.Tasks

let reverseNumber n = 
   new string(n.ToString().ToCharArray() |> Array.rev)

let allDigitsOdd n = 
   if n%2=0 then false
   else
      n.ToString()
      |> Seq.map(fun x -> int x - 0x30)
      |> Seq.forall(fun x -> x%2<>0)

let actImpl n pls loc = 
      if n%10 = 0 then loc
      else
         let reverse = reverseNumber n
         let sum = n + int(reverseNumber n)
         if sum%2<>0 && reverse.[0] <> '0' && allDigitsOdd sum then
            loc + 1
         else
            loc

let mutable counter = 0

Parallel.For(1, 1000000000, (fun () -> 0), actImpl, (fun n -> Interlocked.Add(&counter, n) |> ignore)) |> ignore

printfn "answer = %d" counter