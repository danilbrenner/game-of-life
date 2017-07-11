module Commons

let tee action x = 
    action x |> ignore
    x

let switch someFun noFun =
    function
    | Some i -> someFun i
    | None -> noFun ()
