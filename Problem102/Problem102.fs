type Point = {x: int; y:int; }

let parseTriangle (str:string) = 
   let points = str.Split(',') |> Array.map(fun x -> int(x))
   let a = { x = points.[0]; y = points.[1]}
   let b = { x = points.[2]; y = points.[3]}
   let c = { x = points.[4]; y = points.[5]}
   [|a;b;c;|]

let testTriangle (poly:array<Point>, pt:Point) = 
   let mutable p1 = {x = 0; y = 0};
   let mutable p2 = {x = 0; y = 0};
   let mutable inside = false
   let mutable oldPoint = {x = poly.[poly.Length - 1].x; y = poly.[poly.Length - 1].y}
   for i in [0..poly.Length-1] do
      let newPoint = { x = poly.[i].x; y = poly.[i].y }
      if newPoint.x > oldPoint.x then
         p1 <- oldPoint
         p2 <- newPoint
      else
         p2 <- oldPoint
         p1 <-  newPoint
      if (newPoint.x < pt.x) = (pt.x <= oldPoint.x) && (pt.y - p1.y) * (p2.x - p1.x) < (p2.y - p1.y) * (pt.x - p1.x) then
         inside <- not(inside)
      oldPoint <- newPoint
   inside

let answer = 
   System.IO.File.ReadLines(".\\triangles.txt")
   |> Seq.map(parseTriangle)
   |> Seq.filter(fun x -> testTriangle(x, {x=0; y=0}))
   |> Seq.length

printfn "answer = %d" answer