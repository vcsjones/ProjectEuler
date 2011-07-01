[<StructuralComparison; StructuralEquality>]
type Triangle =
    {
        a : float
        b : float
        c : float
        p : float
    }

let maximum = 1000.0

let rightTriangles = 
    seq {
            for a in [1.0 .. maximum] do
                for b in [1.0 .. maximum] do
                    let c = (a*a + b*b) ** 0.5
                    let p = a + b + c
                    if p <= maximum then
                        if a * a + b * b = c*c then
                            yield {a = a; b = b; c = c; p = p}
                        }
printfn "%f"
    (
        rightTriangles
        |> Seq.distinct
        |> Seq.groupBy(fun x -> x.p)
        |> Seq.maxBy(fun (x, y) -> y |> Seq.length)
        |> fst
    )