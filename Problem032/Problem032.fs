let isPandigital n = 
   let str = new string(n.ToString().ToCharArray() |> Array.sort)
   "123456789" = str

let answer =
    seq {
        for x in 1..50000 do
            for y in 1..(123456789 / x) do
                let product = x * y
                let str = x.ToString() + y.ToString() + product.ToString()
                if str.Length = 9 && isPandigital(str) then yield product
    }
    |> Seq.sum

printfn "%i" answer