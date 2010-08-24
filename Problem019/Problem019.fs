open System

let answer =
   Seq.unfold(fun (x:DateTime) -> Some(x, x.AddDays(1.0))) (DateTime(1901, 1, 1))
   |> Seq.filter(fun x -> x.DayOfWeek = DayOfWeek.Sunday && x.Day = 1)
   |> Seq.takeWhile(fun x -> x <= DateTime(2000, 12, 31))
   |> Seq.toArray
   |> Array.length

printfn "answer = %d" answer