namespace GameOfLife.Model

module Game =
    type Universe = Set<uint>

    let private areValid x y =
        x >= 0 && x <= 65535 && y >= 0 && y <= 65535

    let fromCoords (x: int) (y: int) =
        if areValid x y then
            (uint x <<< 16) ^^^ uint y |> Some
        else
            None

    let toCoords (n: uint) =
        n >>> 16 |> int, (n <<< 16) >>> 16 |> int

    let isCellAlive x y (universe: Universe) =
        fromCoords x y
        |> Option.map (flip Set.contains universe)
        |> Option.defaultWith (fun _ -> false)

    type private Neighbour =
        | Alive of int * int
        | Dead of int * int

    let private unwrapDead =
        function
        | Dead(x, y) -> Some(x, y)
        | _ -> None

    let private unwrapAlive =
        function
        | Alive(x, y) -> Some(x, y)
        | _ -> None

    let private getAt universe (x, y) =
        if isCellAlive x y universe then Alive(x, y) else Dead(x, y)

    let private getNeighbours universe x y =
        [
            x - 1, y - 1
            x - 1, y
            x - 1, y + 1
            x, y - 1
            x, y + 1
            x + 1, y - 1
            x + 1, y
            x + 1, y + 1
        ]
        |> List.filter (fun (x, y) -> areValid x y)
        |> List.map (getAt universe)

    let step (universe: Universe) : Universe =

        let newbornsFolder (acc: Universe) (x, y) =
            let aliveCount =
                getNeighbours universe x y |> List.choose unwrapAlive |> List.length

            match aliveCount, fromCoords x y with
            | 3, Some itm -> Set.add itm acc
            | _ -> acc

        let rec folder (acc: Universe) (x, y) =
            let neighbours = getNeighbours universe x y

            let aliveCount = neighbours |> List.choose unwrapAlive |> List.length

            let newborns = neighbours |> List.choose unwrapDead |> List.fold newbornsFolder acc

            match aliveCount < 2 || aliveCount > 3, fromCoords x y with
            | false, Some itm -> Set.add itm acc |> Set.union newborns
            | _ -> acc |> Set.union newborns

        universe |> Set.toList |> List.map toCoords |> List.fold folder Set.empty
