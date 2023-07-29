module App

open Elmish
open Elmish.React
open Elmish.Debug
open Feliz
open GameOfLife
open GameOfLife.Components
open GameOfLife.Model.User
open Browser

type State = { userSettingsState: UserSettings.State; universeState: Universe.Sate }

type Msg =
    | UserSettingsMsg of UserSettings.Msg
    | UniverseMsg of Universe.Msg

let setDocumentTheme =
    function
    | Dark ->
        document.documentElement.classList.remove "light"
        document.documentElement.classList.add "dark"
    | Light ->
        document.documentElement.classList.remove "dark"
        document.documentElement.classList.add "light"

let init () =
    let settings = Data.getSettings ()
    setDocumentTheme settings.theme
    let userSettingsState, userSettingsCmd = settings |> UserSettings.init
    let universeState, universeCmd = Universe.init ()

    { userSettingsState = userSettingsState; universeState = universeState },
    Cmd.batch [
        userSettingsCmd |> Cmd.map UserSettingsMsg
        universeCmd |> Cmd.map UniverseMsg
    ]

let applySettings settings =
    setDocumentTheme settings.theme
    Data.saveSettings settings
    settings

let update (msg: Msg) (state: State) =
    match msg with
    | UserSettingsMsg subMsg ->
        let state', cmd = UserSettings.update applySettings subMsg state.userSettingsState
        { state with userSettingsState = state' }, cmd
    | UniverseMsg subMsg ->
        let state', cmd = Universe.update subMsg state.universeState
        { state with universeState = state' }, cmd |> Cmd.map UniverseMsg

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        Html.div [
            prop.classes [ "container" ]
            prop.children [
                Html.div [
                    prop.classes [ "side-bar" ]
                    prop.children [
                        UserSettings.render (UserSettingsMsg >> dispatch) state.userSettingsState
                        state.universeState
                        |> Universe.unwrapUniverse
                        |> Option.map Stats.render
                        |> Option.defaultWith (fun _ -> Html.none)
                    ]
                ]
                Universe.render (UniverseMsg >> dispatch) state.universeState
            ]
        ]
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.withSubscription (fun state -> Sub.map "universe" UniverseMsg (Universe.subscribe state.universeState))
|> Program.withDebugger
|> Program.run
