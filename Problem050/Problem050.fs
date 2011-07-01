open System 
open System.Collections.Generic 

let sieve n = 
    seq { 
        yield 2 
        let knownComposites = new HashSet<int>() 
        for i in 3 .. 2 .. n do 
            let found = knownComposites.Contains(i) 
            if not found then 
                yield i 
            do for j in i .. i .. n do 
                   knownComposites.Add(j) |> ignore 
    }