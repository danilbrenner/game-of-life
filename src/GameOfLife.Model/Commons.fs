namespace GameOfLife.Model

open Fable.SimpleJson

[<AutoOpen>]
module Commons =

    let dbgx s v =
        printf $"%s{s} %O{v}"
        v

    let flip f a b = f b a

    let (<?>) r def =
        match r with
        | Ok v -> v
        | Error _ -> def
