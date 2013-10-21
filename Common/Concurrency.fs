namespace Common

open System.Threading;

module Concurrency =
    let interlockAdd (location:int ref) (value:int) : int =
        Interlocked.Add(location, value)