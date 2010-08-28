let numbers = [|0;1;2|]//;3;4;5;6;7;8;9|]

let rotate arr = 
   seq {
      let len = Array.length arr
      for x in [0..len-1] do
      yield arr|>Array.permute(fun i -> (i + x) % len)
   }

let rec permute arr =
   if (Array.length arr) < 1 then
      seq {yield arr}
   else
      let first = Seq.head arr
      let remainder = Seq.skip 1 arr |> Seq.toArray
      let rotateRemainder = rotate remainder
      Seq.map(fun x -> Array.append [|first|] x) rotateRemainder

let r =
   numbers |> permute |> Seq.iter(fun x -> printfn "%A" x)