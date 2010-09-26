let cipherData = 
   System.IO.File.ReadAllText(".\\cipher1.txt").Split(',')
   |> Seq.map(byte) |> Seq.cache

let passwords n =
   let max = int(ceil(float n/3.0))
   seq {
   for x in ['a'..'z'] do
      for y in ['a'..'z'] do
         for z in ['a'..'z'] do
            yield [byte(x);byte(y);byte(z)] |> List.replicate max |> List.concat
   }

let answer =
   seq {
      for pwd in passwords(Seq.length cipherData) do
         let decryptedBytes =
            cipherData
            |> Seq.zip pwd
            |> Seq.map(fun (x, m) -> x ^^^ m)
            |> Seq.toArray
         let text = System.Text.Encoding.ASCII.GetString(decryptedBytes)
         let numberOfSpaces = text |> Seq.filter(fun x -> x = ' ') |> Seq.length
         yield (numberOfSpaces, decryptedBytes)
   }
   |> Seq.maxBy fst
   |> snd
   |> Array.sumBy(fun x -> int(x))


printfn "answer = %d" answer