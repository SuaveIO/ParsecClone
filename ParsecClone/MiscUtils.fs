namespace ParsecClone

open System.Diagnostics

module MiscUtils =
  let time s f =
    let sw = Stopwatch.StartNew()
    let value = f()
    sw.Stop()
    printfn "%s Took %s" s (sw.Elapsed.ToString())
    value