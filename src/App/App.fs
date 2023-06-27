module App

open Elmish
open Elmish.React
open Elmish.Debug
open Feliz
open GameOfLife.Model

type State = {
    scale: float
    position: int * int
    ix: int
}

type Msg =
    | Noop
    | Add

let init () =
    {
        scale = 1.0
        position = (0, 0)
        ix = 0
    },
    Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | Add -> { state with ix = state.ix + 1 }, Cmd.none
    | Noop -> state, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    let text = Say.hello "UserName!!!!"

    Html.div [
        prop.children [
            Html.div [ prop.text text ]
            Html.div [
                prop.onClick (fun _ -> dispatch Add)
                prop.text $"Click me {state.ix}"
            ]
        ]
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.withDebugger
|> Program.run
