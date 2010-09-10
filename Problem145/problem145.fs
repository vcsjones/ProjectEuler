let isReversible n = 
   let str = n.ToString()
//   let digits = str.ToCharArray() |> Array.map(fun x -> int(x) - 0x30)
//   if digits |> Array.forall(fun x -> x%2=0) then
//      false
//   else
   let revInt = int(new string(str.ToCharArray() |> Array.rev))
   (n + revInt).ToString().ToCharArray()
   |> Array.exists(fun x -> (int(x) - 0x30)%2=0)
   |> not
      


let answer = 
   {1..999}
   |> Seq.filter (fun x -> isReversible x)
   |> Seq.iter(fun x -> printfn "%d" x)

//printfn "answer = %d" answer