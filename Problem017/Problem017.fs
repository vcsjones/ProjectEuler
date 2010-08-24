open System.Text.RegularExpressions

let ones = [|"zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"|]
let tens = [|"zero"; "ten"; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety" |]

let lookup =   [|
                  ones
                  tens
                  [|for w in ones -> w + " hundred"|]
                  [|for w in ones -> w + " thousand"|]
               |]

let translate n = 
   let numberStr = n.ToString()
   let rec loop (ns:string, arrPos) = 
      let ordinal = System.Int32.Parse(ns.Substring(0, 1))
      let trans = lookup.[arrPos].[ordinal]
      if Regex.IsMatch(ns, "^(0)+$") then System.String.Empty
      elif ns.Length = 1 then trans
      elif ns.Length = 3 && not(Regex.IsMatch(ns, "^(0)+$")) then trans + " and " + loop(ns.Substring(1), arrPos-1)
      else trans + " " + loop(ns.Substring(1), arrPos-1)
   loop (numberStr, numberStr.Length - 1)


let answer = translate 100
printfn "answer = %s" (answer.TrimEnd())