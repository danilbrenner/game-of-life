module App

open Elmish
open Elmish.React
open Elmish.Debug
open Feliz
open GameOfLife.Components
open GameOfLife.Model.Game

open Browser

type GameMode =
    | Playing
    | Editing

type State = {
    mode: GameMode
    universe: Universe
    userSettingsState: UserSettings.State
}

type Msg =
    | OnCellClicked of int * int
    | ToggleGameMode
    | Step
    | UserSettingsMsg of UserSettings.Msg

let init () =
    let userSettingsState, userSettingsCmd = UserSettings.init

    {
        mode = Editing
        universe = defaultUniverse
        userSettingsState = userSettingsState
    },
    userSettingsCmd |> Cmd.map UserSettingsMsg

let update (msg: Msg) (state: State) =
    match msg with
    | OnCellClicked(x, y) when state.mode = Editing -> { state with universe = toggleCell state.universe x y }, Cmd.none
    | OnCellClicked _ -> state, Cmd.none
    | ToggleGameMode when state.mode = Editing -> { state with mode = Playing }, Cmd.ofMsg Step
    | ToggleGameMode _ -> { state with mode = Editing }, Cmd.none
    | Step when state.mode = Playing ->
        { state with universe = step state.universe }, Cmd.OfAsync.perform (fun _ -> Async.Sleep 300) () (fun _ -> Step)
    | Step -> state, Cmd.none
    | UserSettingsMsg subMsg ->
        let state', cmd = UserSettings.update subMsg state.userSettingsState
        { state with userSettingsState = state' }, cmd


let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        Html.div [
            prop.classes [ "container" ]
            prop.children [
                Html.div [
                    prop.classes [ "side-bar" ]
                    prop.children [
                        UserSettings.render (UserSettingsMsg >> dispatch) state.userSettingsState
                        Stats.render state.universe
                    ]
                ]
                Universe.render (OnCellClicked >> dispatch) state.universe
            ]
        ]
        Html.div [
            prop.classes [ "game-controls" ]
            prop.children [
                Html.button [
                    prop.classes [ "btn"; "round" ]
                    prop.children [
                        (if state.mode = Playing then
                             Html.i [
                                 prop.classes [ "fa-solid"; "fa-stop" ]
                             ]
                         else
                             Html.i [
                                 prop.classes [ "fa-solid"; "fa-play" ]
                             ])
                    ]
                    prop.onClick (fun _ -> dispatch ToggleGameMode)
                ]
            ]
        ]
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.withDebugger
|> Program.run
