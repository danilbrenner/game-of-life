module Commons

let tee f x = 
    f x |> ignore
    x