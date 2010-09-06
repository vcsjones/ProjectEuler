//Skip all triangle numbers. All Triangle numbers are also Hexagonal numbers.

let pentagonal n = n * (3L*n-1L)/2L
let hexagonal n = n * (2L*n-1L)

let pentagonals = set [for n in [166L..100000L] -> pentagonal n]
let hexagonals = set [for n in [143L..100000L] -> hexagonal n]

let answer = 
   pentagonals
   |> Set.intersect hexagonals
   |> Set.minElement

printfn "answer = %d" answer