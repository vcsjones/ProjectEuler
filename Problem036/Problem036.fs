let alpha = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec changeBase num b =
   match num with
   | 0 -> ""
   | _ -> changeBase (num/b) b + string alpha.[num%b]

let isPalindromic str =
   str = new string (str.ToCharArray() |> Array.rev)

let answer = 
   [1..999999]
   |> Seq.map(fun x -> (x, changeBase x 2))
   |> Seq.filter(fun (x,y) -> isPalindromic (string x) && isPalindromic y)
   |> Seq.sumBy fst

printfn "answer = %d" answer