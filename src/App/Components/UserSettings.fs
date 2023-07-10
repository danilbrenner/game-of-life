namespace GameOfLife.Components

open Elmish
open Feliz
open Browser

module UserSettings =

    type State = private State of InnerState
    and private InnerState = { theme: Theme }

    and private Theme =
        | Light
        | Dark

    type Msg = ToggleTheme

    let init = { theme = Dark } |> State

    let update msg (State innerState) =
        match msg with
        | ToggleTheme when innerState.theme = Dark ->
            document.documentElement.classList.remove "dark"
            document.documentElement.classList.add "light"
            State { innerState with theme = Light }, Cmd.none
        | ToggleTheme ->
            document.documentElement.classList.remove "light"
            document.documentElement.classList.add "dark"
            State { innerState with theme = Dark }, Cmd.none

    let render (dispatch: Msg -> unit) (State innerState) =
        Html.div [
            prop.className "card side-element row space-between"
            prop.children [
                Html.div [ prop.text "Hello!" ]
                Html.div [
                    prop.classes [ "pointer" ]
                    prop.onClick (fun _ -> dispatch ToggleTheme)
                    prop.children [
                        if innerState.theme = Dark then
                            Html.i [
                                prop.title "Switch to light theme"
                                prop.classes [ "fa-solid"; "fa-sun" ]
                            ]
                        else
                            Html.i [
                                prop.title "Switch to dark theme"
                                prop.classes [ "fa-solid"; "fa-moon" ]
                            ]
                    ]
                ]
            ]
        ]
