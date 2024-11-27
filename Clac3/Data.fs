module Clac3.Data

module Performance = 
    let measureTime f =
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()
        let result = f()
        sw.Stop()
        (result, sw.Elapsed.TotalMicroseconds)