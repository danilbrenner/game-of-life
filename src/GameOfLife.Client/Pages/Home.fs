namespace GameOfLife.Client.Pages

open Elmish

module Home =

    open Bolero
    open Bolero.Html

    type private InnerState = { txt: string }

    type State = private State of InnerState

    type Msg = Noop

    let init = { txt = "Hi" } |> State

    let update (State state) (msg: Msg) = State state, Cmd.none

    let view (State state) = div { text "Hello!" }
