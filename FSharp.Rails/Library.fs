namespace FSharp.Rails

[<AutoOpen>]
module Common =
    let success = Ok
    let failure = Error

    let either succ fail = function
        | Ok x -> succ x
        | Error e -> fail e
    
    let bind f = either f failure

    let (>>=) x f = bind f x

    let (>=>) f1 f2 = f1 >> bind f2

    let switch f = f >> success

    let map f = either (f >> success) failure

    let tee f x =
        f x |> ignore
        x

    let tryCatch f errHandler x =
        try
            f x |> success
        with
            ex ->  errHandler ex |> failure

    let doubleMap successFunc failureFunc =
        either (successFunc >> success) (failureFunc >> failure)
