let isBouncy n = 
   let nstr = n.ToString() |> Seq.map(fun x -> int x - 0x30)
   let rec loop(s:List<_>, lower, upper) = 
      match s with
      | _ when lower&&upper -> true
      | (x,y) :: tail when x > y -> loop(tail, lower, true)
      | (x,y) :: tail when x < y -> loop(tail, true, upper)
      | _ :: tail -> loop(tail, lower, upper)
      | [] -> lower&&upper
   loop(nstr |> Seq.pairwise |> Seq.toList, false, false)

let answer =
   let rec loop (s:int, numBouncy:decimal) = 
      match s with
      | _ when numBouncy/decimal (s-1)=0.99M -> s-1
      | s when isBouncy s -> loop(s+1, numBouncy+1M)
      | _  -> loop(s+1, numBouncy)
   loop(101, 0M)

printfn "answer = %d" answer