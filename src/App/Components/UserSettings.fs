namespace GameOfLife.Components

open Elmish
open Feliz
open GameOfLife.Components.Universe
open GameOfLife.Model.User

module UserSettings =

    type State = private State of InnerState
    and private InnerState = { settings: Settings }

    type Msg = ToggleTheme

    let init settings =
        { settings = settings } |> State, Cmd.none

    let update saveSettings msg (State innerState) =
        match msg with
        | ToggleTheme ->
            let settings = innerState.settings |> toggleTheme |> saveSettings
            State { innerState with settings = settings }, Cmd.none

    let render (dispatch: Msg -> unit) (State innerState) =
        Html.div [
            prop.className "card side-element row space-between"
            prop.children [
                Html.div [
                    prop.text $"Hello {innerState.settings.name}!"
                ]
                Html.div [
                    prop.classes [ "pointer" ]
                    prop.onClick (fun _ -> dispatch ToggleTheme)
                    prop.children [
                        if innerState.settings.theme = Dark then
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
