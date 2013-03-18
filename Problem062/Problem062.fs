let inline (!!!) x =
   (x |> uint64) * (x |> uint64) * (x |> uint64)

let rec permutations list taken = 
    seq { if Set.count taken = Array.length list then yield [] else
          for l in list do
            if not (Set.contains l taken) then 
              for perm in permutations list (Set.add l taken)  do
                yield l::perm } |> Seq.toList

let max = floor(System.Math.Pow((System.UInt32.MaxValue |> uint64)*1000UL |> float, 1.0/3.0)) |> int
let skip = floor(System.Math.Pow((System.UInt32.MaxValue |> uint64) |> float, 1.0/3.0)) |> int
let cubes = List.init max (fun n -> (n, !!!n)) |> Seq.skip skip |> Seq.toList

let arePermutations (x:uint64) (y:uint64) =
    let sorts x = new string(x.ToString().ToCharArray() |> Array.sort)
    (sorts x) = (sorts y)

let answer =
    cubes
    |> List.find(fun (x, c) -> cubes |> List.filter(fun (n1, c1) -> n1<>x && arePermutations c c1) |> List.length = 4)

printfn "%A" answer