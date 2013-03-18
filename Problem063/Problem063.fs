let (^*) (x:bigint) (n:bigint) =
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

