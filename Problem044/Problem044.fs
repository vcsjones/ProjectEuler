let p n = n*(n*3-1)/2

let pentNums = [for x in [1..10000] -> p x]

let answer =
   seq {
   for x in pentNums do
      for y in pentNums do
         let sum = x + y
         let difference = x-y
         if List.exists(fun x -> x=sum) pentNums && List.exists(fun x -> x=difference) pentNums then
            yield abs(x-y)
   }
   |> Seq.head

printfn "answer = %d" answer