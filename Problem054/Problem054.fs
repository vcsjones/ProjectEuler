type Suit = 
   | Spade
   | Club
   | Heart
   | Diamond

[<CustomEquality; CustomComparison>]
type Card = 
   {
      Value : int;
      Suit : Suit;
   }
   interface System.IComparable with
      member this.CompareTo(other:obj) =
         let card = other :?> Card
         card.Value |> compare this.Value

   override this.Equals(obj:obj) =
      let card = obj :?> Card
      card.Value = this.Value && card.Suit = this.Suit

let highCard (card:List<Card>) = Some((card |> Seq.max).Value)

let xOfAKind (card:List<Card>, x:int) =
   let grouping = card |> Seq.groupBy(fun x -> x.Value) |> Seq.filter(fun (_, x) -> x |> Seq.length > 1)
   if grouping |> Seq.length = 1 && grouping |> Seq.head |> snd |> Seq.length = x then
      Some(grouping |> Seq.head |> fst)
   else
      None

let twoPair(card:List<Card>) = 
   let grouping = card |> Seq.groupBy(fun x -> x.Value)
   let pairs = grouping |> Seq.filter(fun (_, x) -> x |> Seq.length = 2)
   if pairs |> Seq.length = 2 then
      Some(pairs |> Seq.maxBy(fst) |> fst)
   else
      None

let onePair (card:List<Card>) =
   xOfAKind(card, 2)

let threeOfAKind (card:List<Card>) =
   xOfAKind(card, 3)

let fourOfAKind (card:List<Card>) =
   xOfAKind(card, 4)

let flush card = card |> Seq.pairwise |> Seq.forall(fun (x, y) -> x.Suit = y.Suit)
let straight card = card |> List.sort |> Seq.pairwise |> Seq.forall(fun (x, y) -> x.Value + 1 = y.Value)

let isFullHouse (card:List<Card>) =
   let fullHouse = card |> Seq.groupBy(fun x -> x.Value) |> Seq.filter(fun (_, x) -> x |> Seq.length > 1) |> Seq.length = 2
   if fullHouse then Some((card |> Seq.max).Value) else None

let isStraight (card:List<Card>) = 
   if straight(card) then Some((card |> Seq.max).Value) else None

let isFlush (card:List<Card>) =
   if flush(card) then Some((card |> Seq.max).Value) else None

let isStraightFlush (card:List<Card>) =
   if flush(card) && straight(card) then Some((card |> Seq.max).Value) else None

let isRoyalFlush (card:List<Card>) =
   if flush(card) && straight(card) && (card |> Seq.sort |> Seq.head).Value = 10 then Some(14) else None

let cardCompare (a : 'T option when 'T : equality, b : 'T option when 'T : equality) =
   if Option.isNone(a) && Option.isNone(b) then None
   elif Option.isNone(a) && not(Option.isNone(b)) then Some(-1)
   elif not(Option.isNone(a)) && Option.isNone(b) then Some(1)
   elif not(Option.isNone(a)) && not(Option.isNone(b)) then 
      if (Option.get(a) > Option.get(b)) then Some(1)
      elif (Option.get(a) < Option.get(b)) then Some(-1)
      else Some(0)
   else Some(0)

let rec check (a : List<Card>, b : List<Card>, func) =
    match cardCompare(func(a), func(b)) with
    | Some(0) -> check(a, b, highCard)
    | v -> v

let compareHands (hand1:List<Card>, hand2:List<Card>) =
   seq {
      yield check(hand1, hand2, isRoyalFlush)
      yield check(hand1, hand2, isStraightFlush)
      yield check(hand1, hand2, fourOfAKind)
      yield check(hand1, hand2, isFullHouse)
      yield check(hand1, hand2, isFlush)
      yield check(hand1, hand2, isStraight)
      yield check(hand1, hand2, threeOfAKind)
      yield check(hand1, hand2, twoPair)
      yield check(hand1, hand2, onePair)
      yield cardCompare(highCard(hand1), highCard(hand2))
   } |> Seq.choose(fun x -> x) |> Seq.head

[<StructuralEquality; CustomComparison>]
type Hand = 
   {
      Cards : List<Card>;
   }
   interface System.IComparable with
      member this.CompareTo(other:obj) =
         let hand2 = other :?> Hand
         compareHands(this.Cards, hand2.Cards)


type Set =
   {
      Number : int;
      Player1 : Hand;
      Player2 : Hand;
   }

let parseCard (str:string) =
   {
      Value =
         match str.[0] with
         | 'T' -> 10
         | 'J' -> 11
         | 'Q' -> 12
         | 'K' -> 13
         | 'A' -> 14
         | v -> int(v.ToString())
      Suit =
         match str.[1] with
         | 'S' -> Suit.Spade
         | 'C' -> Suit.Club
         | 'D' -> Suit.Diamond
         | 'H' -> Suit.Heart
         | suit -> invalidArg "suit" (sprintf "Value passed in was %c." suit)
   };


let hands = 
   System.IO.File.ReadLines(".\poker.txt")
   |> Seq.map(fun x -> x.Split(' '))
   |> Seq.mapi(fun i x -> {
                           Number = i + 1
                           Player1 = {Cards = x |> Seq.take 5 |> Seq.map(fun y -> parseCard(y)) |> Seq.toList};
                           Player2 = {Cards = x |> Seq.skip 5 |> Seq.take 5 |> Seq.map(fun y -> parseCard(y)) |> Seq.toList};
                       })
   |> Seq.filter (fun x -> x.Player1 > x.Player2)
   |> Seq.length
   (*|> Seq.iter(fun x -> printfn "%s" (if x.Player1 > x.Player2 then "Player 1 wins set " + string(x.Number) else "Player 2 wins set " + string(x.Number)))*)

printfn "%i" hands