open System.Threading
open System.Threading.Tasks

let divisors x =
   [1L..x|>float|>sqrt|>int64]
   |> Seq.filter(fun y -> x%y=0L)
   |> Seq.map(fun y -> [y;x/y])
   |> Seq.concat
   |> Seq.filter(fun y -> x<>y && y <> 1L)
   |> Seq.toList

let isPandigital n = 
   let str = new string(n.ToString().ToCharArray() |> Array.sort)
   "123456789" = str

let pandigitalIdentity n pls loc = 
      let d = divisors n
      let rec loop l = 
         match l with
         | head :: tail ->
            let f = tail |> List.tryFind(fun x -> isPandigital(string x + string head + string(x*head)))
            match f with
            | None -> loop(tail)
            | _ -> loc + n
         | [] -> loc
      loop d

let answer = ref 0L

Parallel.For(1L, 500000000L, (fun () -> 0L), pandigitalIdentity, (fun x -> Interlocked.Add(answer, x) |> ignore)) |> ignore

printfn "answer = %d" !answer