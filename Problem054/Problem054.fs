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
   //Assume that all suits are considered equal
   interface System.IComparable with
      member this.CompareTo(other:obj) =
         let card = other :?> Card
         card.Value |> compare this.Value

   override this.Equals(obj:obj) =
      let card = obj :?> Card
      card.Value = this.Value && card.Suit = this.Suit

   override this.GetHashCode() = 
      (this.Value * 251) + this.Suit.GetHashCode()

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

type Hand(list : List<Card>) = 
   let cards = list |> List.sortBy(fun x -> x.Value) |> List.rev
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
      let grouping =
         this.Cards
         |> List.toSeq
         |> Seq.groupBy(fun x -> x.Value)
         |> Seq.filter(fun (_, v) -> v |> Seq.length = 2)
      if grouping |> Seq.length = 1 then Some(grouping |> Seq.head |> fst) else None

   member this.TwoPair : int option =
      let grouping = 
         this.Cards
         |> List.toSeq
         |> Seq.groupBy(fun x -> x.Value)
         |> Seq.filter(fun (_, v) -> v |> Seq.length = 2)
      if grouping |> Seq.length = 2 then Some(grouping |> Seq.maxBy(fun (x, y) -> x) |> fst) else None

   member this.ThreeOfAKind : int option =
      let grouping =
         this.Cards
         |> List.toSeq
         |> Seq.groupBy(fun x -> x.Value)
         |> Seq.filter(fun (_, v) -> v |> Seq.length = 3)
      if grouping |> Seq.length = 1 then Some(grouping |> Seq.head |> fst) else None

   member this.Straight : int option =
      let rec check list next original = 
         match list with
         | head :: tail when head.Value = next -> check tail (head.Value-1) original
         | [] -> Some(original)
         | _ -> None
      check this.Cards this.Cards.Head.Value this.Cards.Head.Value
      
   member this.Flush : int option =
      if this.Cards |> List.forall(fun x -> x.Suit = this.Cards.Head.Suit) then Some(this.Cards.Head.Value) else None

   member this.FullHouse : int option =
      let grouping = 
         this.Cards
         |> List.toSeq
         |> Seq.groupBy(fun x -> x.Value)
      let two = grouping |> Seq.filter(fun (_, v) -> v |> Seq.length = 2)
      let three = grouping |> Seq.filter(fun (_, v) -> v |> Seq.length = 3)
      if two |> Seq.length = 1 && three |> Seq.length = 1 then Some(three |> Seq.head |> fst) else None

   member this.FourOfAKind : int option =
      let grouping =
         this.Cards
         |> List.toSeq
         |> Seq.groupBy(fun x -> x.Value)
         |> Seq.filter(fun (_, v) -> v |> Seq.length = 4)
      if grouping |> Seq.length = 1 then Some(grouping |> Seq.head |> fst) else None

   member this.StraightFlush : int option =
      let s = [this.Straight; this.Flush];
      if s |> List.forall Option.isSome then s |> List.maxBy(fun x -> Option.get x) else None

   member this.RoyalFlush : int option =
      if this.StraightFlush |> Option.exists(fun x -> x = 14) then Some(14) else None
   interface System.IComparable with
      member this.CompareTo(otherHand:obj) =
         let compararo (left:int option) (right:int option) : (int option) = 
            
         let other = otherHand :?> Hand
         


type Set =
   {
      Number : int;
      Player1 : Hand;
      Player2 : Hand;
   }

let testHand = new Hand([{Suit = Spade; Value = 10}; {Suit = Spade; Value = 11};
                           {Suit = Spade; Value = 12}; {Suit = Spade; Value = 13}; {Suit = Spade; Value = 14}])

printfn "%A" (testHand.Score)
(*
let hands = 
   System.IO.File.ReadLines(".\poker.txt")
   |> Seq.map(fun x -> x.Split(' '))
   |> Seq.mapi(fun i x -> {
                           Number = i + 1
                           Player1 = new Hand(x |> Seq.take 5 |> Seq.map(fun y -> parseCard(y)) |> Seq.toList);
                           Player2 = new Hand(x |> Seq.skip 5 |> Seq.take 5 |> Seq.map(fun y -> parseCard(y)) |> Seq.toList);
                       })
   |> Seq.filter (fun x -> x.Player1 > x.Player2)
*)