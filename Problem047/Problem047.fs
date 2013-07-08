let factors (n:int) =
    [1..n|>float|>sqrt|>int]
    |> Seq.filter(fun y -> n%y=0)
    |> Seq.collect(fun y -> [y;n/y])
