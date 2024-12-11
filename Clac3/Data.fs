module Clac3.Data

module Performance = 
    let measureTime f =
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()
        let result = f()
        sw.Stop()
        result, sw.Elapsed.TotalMilliseconds

    let measureAverageTime n f =
        let result, timeFirst = measureTime f
        let mutable time = timeFirst
        for _ in [1..n] do 
            let _, timeNext = measureTime f
            time <- time + timeNext
        result, time