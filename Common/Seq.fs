namespace Common

module Array2D =
    let flatten(source:'T[,]) : 'T[] = 
        seq {
            for d1 in [0.. (source |> Array2D.length1)-1] do
                for d2 in [0 .. (source |> Array2D.length2)-1] do
                    yield source.[d1, d2]
        }
        |> Seq.toArray