type Card = 
   {
      Value : int;
      Suit : char;
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
      Suit = str.[1] 
   };

type Hand(list : List<Card>) = 
   let cards = list |> List.sortBy(fun x -> x.Value) |> List.rev
   let singleGroupOfSize size =
      let grouping =
         cards
         |> List.toSeq
         |> Seq.groupBy(fun x -> x.Value)
         |> Seq.filter(fun (_, v) -> v |> Seq.length = size)
      if grouping |> Seq.length = 1 then Some(grouping |> Seq.head |> fst) else None

   member this.Cards
      with get() : List<Card> = cards

   member this.HighCard with get() : Card = cards |> List.head

   member this.HasHighest (other : Hand) : bool =
      let head = this.Cards
                  |> List.zip other.Cards
                  |> List.toSeq
                  |> Seq.skipWhile(fun (t, o) -> t.Value = o.Value)
                  |> Seq.head
      (head |> snd) > (head |> fst)

   member this.OnePair : int option =
      singleGroupOfSize 2

   member this.TwoPair : int option =
      let grouping = 
         this.Cards
         |> List.toSeq
         |> Seq.groupBy(fun x -> x.Value)
         |> Seq.filter(fun (_, v) -> v |> Seq.length = 2)
      if grouping |> Seq.length = 2 then Some(grouping |> Seq.maxBy(fun (x, _) -> x) |> fst) else None

   member this.ThreeOfAKind : int option =
      singleGroupOfSize 3

   member this.Straight : int option =
      let rec check list next original = 
         match list with
         | head :: tail when head.Value = next -> check tail (head.Value-1) original
         | [] -> Some(original)
         | _ -> None
      check this.Cards this.Cards.Head.Value this.Cards.Head.Value
      
   member this.Flush : int option =
      if this.Cards |> List.forall(fun x -> x.Suit = this.Cards.Head.Suit) then Some this.Cards.Head.Value else None

   member this.FullHouse : int option =
      let grouping = this.Cards |> List.toSeq |> Seq.groupBy(fun x -> x.Value)
      let two = grouping |> Seq.filter(fun (_, v) -> v |> Seq.length = 2)
      let three = grouping |> Seq.filter(fun (_, v) -> v |> Seq.length = 3)
      if two |> Seq.length = 1 && three |> Seq.length = 1 then Some(three |> Seq.map fst |> Seq.max) else None

   member this.FourOfAKind : int option =
      singleGroupOfSize 4

   member this.StraightFlush : int option =
      let s = [this.Straight; this.Flush];
      if s |> List.forall Option.isSome then s |> List.maxBy Option.get else None

   member this.RoyalFlush : int option =
      if this.StraightFlush |> Option.exists(fun x -> x = 14) then Some 14 else None
         
let compareHands (hand1 : (int * Hand)) (hand2 : (int * Hand)) : int =
   let compraro (applier : Hand -> int option) : int option =
      let firstHand = snd hand1
      let secondHand = snd hand2
      if firstHand |> applier |> Option.isSome && secondHand |> applier |> Option.isSome then
         let firstValue = firstHand |> applier |> Option.get
         let secondValue = secondHand |> applier |> Option.get
         if firstValue = secondValue then
            if firstHand.HasHighest secondHand then Some(fst hand1) else Some(fst hand2)
         elif firstValue > secondValue then Some(fst hand1)
         else Some(fst hand2)
      elif firstHand |> applier |> Option.isSome && secondHand |> applier |> Option.isNone then Some(fst hand1)
      elif firstHand |> applier |> Option.isNone && secondHand |> applier |> Option.isSome then Some(fst hand2)
      else None
   seq {
      yield compraro (fun x -> x.RoyalFlush)
      yield compraro (fun x -> x.StraightFlush)
      yield compraro (fun x -> x.FourOfAKind)
      yield compraro (fun x -> x.FullHouse)
      yield compraro (fun x -> x.Flush)
      yield compraro (fun x -> x.Straight)
      yield compraro (fun x -> x.ThreeOfAKind)
      yield compraro (fun x -> x.TwoPair)
      yield compraro (fun x -> x.OnePair)
      yield compraro (fun x -> Some(x.HighCard.Value))
   } |> Seq.pick(fun x -> x)
   

let hands = 
   System.IO.File.ReadLines(".\poker.txt")
   |> Seq.map(fun x -> x.Split(' '))
   |> Seq.map(fun x -> (
                           new Hand(x |> Seq.take 5 |> Seq.map parseCard |> Seq.toList),
                           new Hand(x |> Seq.skip 5 |> Seq.take 5 |> Seq.map parseCard |> Seq.toList);
                       ))
   |> Seq.filter (fun x -> compareHands (1, fst x) (2, snd x) = 1)

printfn "Player 1 winnings: %i" (hands |> Seq.length)