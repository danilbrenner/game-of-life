namespace GameOfLife.Components

open Browser.Types
open Feliz
open GameOfLife.Model.Game
open Microsoft.FSharp.Core

module Universe =
    let private cellSize = 16

    let viewCell ix =
        let x, y = toCoords ix

        Svg.rect [
            svg.x (x * cellSize)
            svg.y (y * cellSize)
            svg.height (cellSize - 1)
            svg.width (cellSize - 1)
        ]

    let onClick onCellClicked (mouseEvent: MouseEvent) =
        let toCellCoord shift position =
            (position - shift) / float cellSize |> int

        (toCellCoord 320.0 mouseEvent.clientX, toCellCoord 11.0 mouseEvent.clientY)
        |> onCellClicked

    let render (onCellClicked: int * int -> unit) { cells = cells } =
        Svg.svg [
            svg.id "universe"
            svg.classes [ "card game" ]
            svg.onMouseDown (onClick onCellClicked)
            svg.children (cells |> Set.toList |> List.map viewCell)
        ]
