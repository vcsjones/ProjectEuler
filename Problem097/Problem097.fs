let fastPow(x:bigint,n:bigint) =
   let mutable result = 1I
   let mutable xm = x
   let mutable nm = n
   while not nm.IsZero do
      if not nm.IsEven then
         result <- result * xm
         nm <- nm-1I
      xm <- xm**2
      nm <- nm/2I
   result

let answer = 
   let str = (28433I*fastPow(2I,7830457I)+1I).ToString("R")
   str.[(str.Length - 10)..]

printfn "test = %s" answer
