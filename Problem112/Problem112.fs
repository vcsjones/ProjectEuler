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
   let rec loop (s:List<int>, numBouncy:decimal) = 
      match s with
      | head :: tail when numBouncy/decimal (head-1)=0.99M -> head-1
      | head :: tail when isBouncy head -> loop(tail, numBouncy+1M)
      | _ :: tail -> loop(tail, numBouncy)
      | [] -> failwith("Unable to solve")
   loop([101..9999999], 0M)

printfn "answer = %d" answer