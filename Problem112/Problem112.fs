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
   let rec loop (s:List<int>, count:decimal, numBouncy:decimal) = 
      match s with
      | head :: tail when numBouncy/count=0.99M -> head-1
      | head :: tail when isBouncy head -> loop(tail,count+1M, numBouncy+1M)
      | _ :: tail -> loop(tail, count+1M, numBouncy)
      | [] -> failwith("Unable to solve")
   loop([2..9999999], 1M, 0M)

printfn "answer = %d" answer