module Game

let private countNeighbours (universe: bool array) index =
    [|
        universe.[index+1]
        universe.[index+70]
        universe.[index+71]
        universe.[index+72]
        universe.[index-1]
        universe.[index-70]
        universe.[index-71]
        universe.[index-72]
    |]
    |> Array.filter id
    |> Array.length

let private isAlive st =
    function
    | n when n = 2 && st -> true
    | n when n = 3 -> true
    | _ -> false 

let step (universe: bool array) =
    let cn = countNeighbours universe
    universe
    |> Array.mapi (fun i x -> cn i |> isAlive x )