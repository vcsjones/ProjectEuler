let counter upto = 
   let rec loop n i = 
      if i > upto then 1
      else
         let numbers =
            Seq.unfold(fun x -> Some(x, x+i)) n
            |> Seq.skip 1
            |> Seq.take 4
            |> Seq.toList
            |> List.rev
         let sum = numbers |> List.sum
         let head = numbers |> List.head
         sum + loop head (i+2)
   loop 1 2

let answer = counter 1001

printfn "answer = %d" answer