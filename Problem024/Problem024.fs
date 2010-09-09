let numbers = [|0;1;2|]//;3;4;5;6;7;8;9|]

let rotate arr = 
   seq {
      let len = Array.length arr
      for x in [0..len-1] do
      yield arr |> Array.permute(fun i -> (i + x) % len)
   }

let rec permute (arr:array<int>) =
   let head = [|arr.[0]|]
   let remainder = arr.[1..]
   let rotateRemainder = remainder |> rotate
   seq {
      for x in rotateRemainder do
         let permuteRemainder = permute x
         yield! permuteRemainder
   }

let r =
   numbers |> permute |> Seq.iter(fun x -> printfn "%A" x)