module App

open Elmish
open Elmish.React
open Elmish.Debug
open Feliz
open GameOfLife.Components
open GameOfLife.Model.Game

open Browser

type State = { userSettingsState: UserSettings.State; universeState: Universe.Sate }

type Msg =
    | UserSettingsMsg of UserSettings.Msg
    | UniverseMsg of Universe.Msg

let init () =
    let userSettingsState, userSettingsCmd = UserSettings.init
    let universeState, universeCmd = Universe.init ()

    { userSettingsState = userSettingsState; universeState = universeState },
    Cmd.batch [
        userSettingsCmd |> Cmd.map UserSettingsMsg
        universeCmd |> Cmd.map UniverseMsg
    ]

let update (msg: Msg) (state: State) =
    match msg with
    | UserSettingsMsg subMsg ->
        let state', cmd = UserSettings.update subMsg state.userSettingsState
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
        Html.div [
            prop.classes [ "game-controls" ]
            prop.children [
                Html.button [
                    prop.classes [ "btn"; "round" ]
                    prop.children [
                        (if Universe.isPlaying state.universeState then
                             Html.i [
                                 prop.classes [ "fa-solid"; "fa-stop" ]
                             ]
                         else
                             Html.i [
                                 prop.classes [ "fa-solid"; "fa-play" ]
                             ])
                    ]
                    prop.onClick (fun _ -> Universe.Msg.ToggleGameMode |> UniverseMsg |> dispatch)
                ]
            ]
        ]
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.withSubscription (fun state -> Sub.map "universe" UniverseMsg (Universe.subscribe state.universeState))
|> Program.withDebugger
|> Program.run
