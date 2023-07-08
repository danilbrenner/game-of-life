namespace GameOfLife.Components

open Feliz
open GameOfLife.Model.Game

module Stats =
    let render (universe: Universe) =
        Html.div [
            prop.className "card side-element"
            prop.children [
                Html.div [
                    prop.text $"Generations passed: {universe.generations}"
                ]
                Html.div [
                    prop.text $"Cells alive: {universe.cells |> Set.count}"
                ]
            ]
        ]
