open System.Text
open System

let divide n = 
    let inline toInt (x:char) = int x - 0x30
    let rec solve (sb:StringBuilder, l:int, iter:int) = 
        if iter = 4000 then sb
        else 
            let result = l / n
            sb.Append(box result) |> ignore
            let multiplyBack = result * n
            let difference = l - multiplyBack
            assert (difference >= 0)
            let diffMul = difference * 10
            solve(sb, diffMul, iter+1)
    let answer = solve (new StringBuilder(), 1, 1)
    answer.ToString(1, answer.Length - 1).ToCharArray() |> List.ofArray

let compare (a:seq<_>) (b:seq<_>) =
    let equal = Seq.forall2(fun x y -> x = y)
    equal a b

let getRepGroup (chrs:List<char>) = 
    let rec windowGroup (group:List<char>) (running:List<List<char>>) = 
        match group with
        | list when list |> List.length = 1 -> windowGroup list.Tail running //One character in length. Can't repeat, so skip.
        | list -> 
            List.Empty
        | [] -> running |> List.maxBy List.length
    windowGroup chrs List.Empty

(* let answer = 
    [1..999]
    |> Seq.map(fun x -> (x, x |> divide |> countMaxRep))
    |> Seq.maxBy snd
    |> fst *)

//printfn "answer = %i" answer

let six = divide 6
printfn "%s" (new string(six |> Seq.toArray))