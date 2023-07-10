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

    [<Literal>]
    let private lightStr = "light"

    [<Literal>]
    let private darkStr = "dark"

    let toggleTheme (fromTheme: string) (toTheme: string) =
        document.documentElement.classList.remove fromTheme
        document.documentElement.classList.add toTheme
        window.localStorage.setItem ("ui-theme", toTheme)

    type Msg = ToggleTheme

    let init =
        let theme = window.localStorage.getItem "ui-theme"
        { theme = if theme = lightStr then Dark else Light } |> State, Cmd.ofMsg ToggleTheme

    let update msg (State innerState) =
        match msg with
        | ToggleTheme when innerState.theme = Dark ->
            toggleTheme darkStr lightStr
            State { innerState with theme = Light }, Cmd.none
        | ToggleTheme ->
            toggleTheme lightStr darkStr
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
