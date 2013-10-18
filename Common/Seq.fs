namespace Common
open System

module Seq =
    let equal (source : seq<'T> when 'T : equality) (against : seq<'T>) : bool = 
        (Seq.zip source against) |> Seq.forall(fun (x, y) -> x = y)