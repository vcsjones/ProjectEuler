namespace Common

module Timer =
    let duration (name : string) (func : unit -> 'T) : 'T =
        let timer = System.Diagnostics.Stopwatch.StartNew()
        let result = func()
        timer.Stop()
        printfn "%s -> Elapsed: %A" name timer.Elapsed
        result
