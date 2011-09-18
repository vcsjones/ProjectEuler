open System.Collections.Generic 

type Solution =
    {
        prime:int;
        square:int;
    }
    override this.ToString() = 
        System.String.Format("{0} + 2 * {1}²", this.prime, this.square)

let isPrime n = [|2.. (n|>float|>sqrt|>int)|] |> Array.exists(fun y -> n <> y && n % y=0) |> not

let odds = 
    Seq.unfold(fun x -> Some(x, x+2)) 1
    |> Seq.filter(fun x -> x |> isPrime |> not)

let sieve n = 
    seq { 
        yield 2 
        let knownComposites = new HashSet<int>() 
        for i in 3 .. 2 .. n do 
            let found = knownComposites.Contains(i) 
            if not found then 
                yield i 
            do for j in i .. i .. n do 
                   knownComposites.Add(j) |> ignore
   }

let primes = sieve 10000 |> Seq.cache

let solve n = 
    let rec exp i p = 
        match i with
        | x when x = n-p -> None
        | x when n = p + 2 * (i*i) -> Some({prime = p; square = i;})
        | _ -> exp (i+1) p
    primes
    |> Seq.takeWhile(fun x -> x < n)
    |> Seq.tryPick(fun p -> exp 1 p)

let answer = 
    odds
    |> Seq.skipWhile(fun x -> x |> solve |> Option.isSome)
    |> Seq.head

printfn "answer = %i" answer