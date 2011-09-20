open System.Text.RegularExpressions

let ones = [|""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"|]
let tens = [|""; "ten"; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety"|]
let teen = [|"ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"|]

let wordify n = 
    let word = 
        let rec dev s i = 
            let str = n.ToString()
            if (i = str.Length) then s
            else
                let digit = int(str.Substring(str.Length - 1 - i, 1))
                let remainder = int(str.Substring(str.Length - 1 - i))
                let mutable build = s
                let word = 
                    match i with
                    | 0 -> ones.[digit]
                    | 1 -> 
                            match tens.[digit] with
                            | "ten" -> build<-""; teen.[int(str.Substring(str.Length - i, 1))]
                            | a -> a
                    | 2 -> ones.[digit] + " hundred and"
                    | 3 -> ones.[digit] + " thousand"
                dev (word + " " + build) (i+1)
        dev "" 0
    if n = 1000 then "one thousand" else
        let mutable trim = word.Trim()
        if trim.EndsWith(" and") then trim.Substring(0, trim.Length - 4) else trim

let count (n:string) = 
    n.Replace(" ", "").Replace(" ", "").Length

let answer = 
    [1..1000]
    |> Seq.map wordify
    |> Seq.map count
    |> Seq.sum

printfn "answer = %i" answer