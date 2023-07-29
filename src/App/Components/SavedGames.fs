namespace GameOfLife.Components

open System
open Feliz

module SavedGames =

    let private renderGameItem removeGame loadGame (id: Guid, name: string) =
        Html.div [
            prop.classes [ "row space-between" ]
            prop.children [
                Html.div [
                    prop.classes [ "pointer" ]
                    prop.onClick (fun _ -> loadGame id)
                    prop.text name
                ]
                Html.div [
                    prop.classes [ "pointer" ]
                    prop.onClick (fun _ -> removeGame id)
                    prop.children [
                        Html.i [
                            prop.classes [ "fa-solid"; "fa-trash" ]
                        ]
                    ]
                ]
            ]
        ]

    let render removeGame loadGame (games: (Guid * string) list) =
        Html.div [
            prop.className "card side-element saved-games"
            prop.children (games |> List.map (renderGameItem removeGame loadGame))
        ]
