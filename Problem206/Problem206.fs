open System.Text.RegularExpressions

let reg = new Regex(@"^1\d2\d3\d4\d5\d6\d7\d8\d9\d0$", RegexOptions.Compiled)

let answer = 
   seq {1000000000UL..10UL..1389026623UL}
   |> Seq.filter(fun x -> reg.IsMatch((x*x).ToString()))
   |> Seq.head

printfn "answer = %d" answer