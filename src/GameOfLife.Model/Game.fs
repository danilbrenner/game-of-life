namespace GameOfLife.Model

module Game =
    type Universe = {
        generations: int
        cells: Set<uint>
    }

    let defaultUniverse = {
        generations = 0
        cells = Set.empty
    }

    let private areValid x y =
        x >= 0 && x <= 65535 && y >= 0 && y <= 65535

    let fromCoords (x: int) (y: int) =
        if areValid x y then
            (uint x <<< 16) ^^^ uint y |> Some
        else
            None

    let toCoords (n: uint) =
        n >>> 16 |> int, (n <<< 16) >>> 16 |> int

    let private isAlive x y cells =
        fromCoords x y
        |> Option.map (flip Set.contains cells)
        |> Option.defaultWith (fun _ -> false)

    let isCellAlive x y ({ cells = cells }: Universe) = isAlive x y cells

    let toggleCell (universe: Universe) x y =
        match fromCoords x y with
        | Some ix when Set.contains ix universe.cells -> { universe with cells = Set.remove ix universe.cells }
        | Some ix -> { universe with cells = Set.add ix universe.cells }
        | None -> universe

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

    let private getAt cells (x, y) =
        if isAlive x y cells then Alive(x, y) else Dead(x, y)

    let private getNeighbours cells x y =
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
        |> List.map (getAt cells)

    let step (universe: Universe) : Universe =

        let newbornsFolder acc (x, y) =
            let aliveCount =
                getNeighbours universe.cells x y |> List.choose unwrapAlive |> List.length

            match aliveCount, fromCoords x y with
            | 3, Some itm -> Set.add itm acc
            | _ -> acc

        let rec folder (acc: Set<uint>) (x, y) =
            let neighbours = getNeighbours universe.cells x y

            let aliveCount = neighbours |> List.choose unwrapAlive |> List.length

            let newborns = neighbours |> List.choose unwrapDead |> List.fold newbornsFolder acc

            match aliveCount < 2 || aliveCount > 3, fromCoords x y with
            | false, Some itm -> Set.add itm acc |> Set.union newborns
            | _ -> acc |> Set.union newborns

        {
            universe with
                cells = universe.cells |> Set.toList |> List.map toCoords |> List.fold folder Set.empty
                generations = universe.generations + 1
        }
