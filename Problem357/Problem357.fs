open System.Collections.Concurrent
open System
open System.Threading
open System.Threading.Tasks

let divisors x =
   [1..x|>float|>sqrt|>int]
   |> Seq.filter(fun y -> x%y=0)
   |> Seq.map(fun y -> [y;x/y])
   |> Seq.concat
   |> Seq.filter(fun y -> x<>y)

let set = new ConcurrentDictionary<int, bool>()
let isPrime n =
    let value = ref false
    if set.TryGetValue(n, value) then
        !value
    else
        let prime = [|2.. (n|>float|>sqrt|>int)|] |> Array.exists(fun y -> n <> y && n % y=0) |> not
        set.TryAdd(n, prime) |> ignore
        prime

let answer = ref 0
let count = ref 0
let watch = System.Diagnostics.Stopwatch.StartNew()

let actImpl n pls loc = 
    if !count % 100000 = 0 && !count <> 0 then
        printfn "%f complete" (float(!count) / 100000000.0 * 100.0)
        printfn "%A elapsed" (watch.Elapsed)
    let result = divisors(n) |> Seq.exists(fun z -> isPrime (z+n/z) |> not) |> not
    Interlocked.Increment(count) |> ignore
    match result with
    | true -> n
    | _ -> 0

let merge n =
    Interlocked.Add(answer, n) |> ignore

let options = new ParallelOptions()
options.MaxDegreeOfParallelism <- 8
Parallel.For(1, 100000001, options, (fun () -> 0), actImpl, merge) |> ignore

printfn "%i" !answer