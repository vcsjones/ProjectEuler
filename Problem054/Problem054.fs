let alpha = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec changeBase num b =
   match num with
   | 0 -> ""
   | _ -> changeBase (num/b) b + string alpha.[num%b]

let hands = seq {
                     //yield "8C TS KC 9H 4S    7D 2S 5D 3S AC"
                     yield "5C AD 5D AC 9C    7C 5H 8D TD KS"
                }

let parseCard (card:string) =
   assert(card.Length=2)
   let value = card.[0]
   let suit = card.[1]
   (
         match value with
         | value when ['2'..'9'] |> List.exists(fun x -> x = value) -> int(value) - 0x30
         | 'T' -> 10
         | 'J' -> 11
         | 'Q' -> 12
         | 'K' -> 13
         | 'A' -> 14
         | _ -> failwith "Unkown card encountered"
      ,
      suit
   )

let parseHand (hand:string) =
   let split = hand.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
   (split.[0..4] |> Array.map parseCard, split.[5..9] |> Array.map parseCard)

let manyOfAKind hand n =
      hand
      |> Seq.groupBy fst
      |> Seq.filter(fun (k, s) -> (s |> Seq.toArray) |> Array.length = n)
      |> Seq.toArray

let pairValue (hand:array<(int * char)>) =
   let pairs = manyOfAKind hand 2
   if pairs.Length <> 1 then 0 else (fst pairs.[0])

let twoPairValue (hand:array<(int * char)>) =
   let pairs = manyOfAKind hand 2
   if pairs.Length <> 2 then 0 else (pairs |> Array.map fst |> Array.max)

let threeOfAKindValue (hand:array<(int * char)>) =
   let triple = manyOfAKind hand 3
   if triple.Length <> 1 then 0 else (fst triple.[0])

let scoreHand (hand:array<(int * char)>) =
   let highestCard = hand |> Array.map fst |> Array.max
   let twoOfAKind = hand |> pairValue
   let twoPair = hand |> twoPairValue
   (highestCard ||| (twoOfAKind <<< 4)) ||| (twoPair <<< 8)


let parsedHands =
   hands
   |> Seq.map parseHand
   |> Seq.map (fun (a, b) -> (scoreHand a, scoreHand b))
   |> Seq.iter(fun (a, b) -> printfn "%s | %s" (changeBase a 2) (changeBase b 2))