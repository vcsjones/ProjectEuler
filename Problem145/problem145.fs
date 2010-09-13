let reverseNumber n = 
   int(new string(n.ToString().ToCharArray() |> Array.rev))

let allDigitsOdd n = 
   n.ToString().ToCharArray()
   |> Seq.map(fun x -> int(x) - 0x30)
   |> Seq.forall(fun x -> x%2<>0)

let mutable counter = 0

let asyncFilter n = 
      let sum = n + reverseNumber n
      if sum%2<>0 && allDigitsOdd sum then
        counter <- counter+1

let answer = 
   {1..999999999}
   |> Seq.filter(fun x -> x%10<>0)
   |> Seq.iter(fun x -> asyncFilter x)

printfn "answer = %d" counter