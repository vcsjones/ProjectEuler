let answer = 
   System.IO.File.ReadLines(".\\triangle.txt")
   |> Seq.map(fun x -> x.Split(' ') |> Seq.map(int))
   |> Seq.fold(fun acc x -> 
            let rowLen = Seq.length x
            x |> Seq.mapi(fun i y -> 
                              let current = fun () -> y+(Seq.nth i acc)
                              let previous = fun () -> y+(Seq.nth (i-1) acc)
                              if i=0 then current()
                              elif i=rowLen-1 then previous()
                              else max (current()) (previous())
                           )
              |> Seq.toList
         ) [0]
   |> List.max

printfn "answer = %d" answer