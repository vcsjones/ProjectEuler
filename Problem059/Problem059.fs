let cipherData = 
   System.IO.File.ReadAllText(".\\cipher1.txt").Split(',')
   |> Array.map(fun x -> x.Trim() |> byte)

let passwords = seq {
   for x in ['a'..'z'] do
      for y in ['a'..'z'] do
         for z in ['a'..'z'] do
            yield [|byte(x);byte(y);byte(z)|]
   }


let specMod n = 
   match n % 3 with
   | 1 -> 0
   | 2 -> 1
   | 0 -> 2
   | _ -> failwith("WTF")

let answer =
   seq {
      for pwd in passwords do
         let decryptedBytes = 
            cipherData
            |> Array.mapi(fun i x -> pwd.[specMod (i+1)] ^^^ x)
         let text = System.Text.Encoding.ASCII.GetString(decryptedBytes)
         if text.Contains("the") && text.Contains("was") && text.Contains("so") && text.EndsWith(".") then
            yield decryptedBytes |> Array.fold(fun acc x -> acc + int(x)) 0
   }
   |> Seq.head


printfn "answer = %d" answer