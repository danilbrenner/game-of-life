namespace GameOfLife.Components

open System
open Browser.Types
open Elmish
open Feliz
open Feliz.prop
open GameOfLife.Model.Game
open Microsoft.FSharp.Core
open Browser

module Universe =

    [<Literal>]
    let private cellSize: int = 10

    type Message =
        | OnMouseDown of MouseEvent
        | OnMouseUp of MouseEvent
        | GetUniverseSize of int
        | GotUniverseSize of int * int
        | ToggleGameMode
        | Step
        | OnResize
        | OnKeyDown of KeyboardEvent
        | Noop
        | SaveGame
        | CancelSaveGame
        | ProceedSaveGame
        | SaveGameNameChange of string

    type UpdateDependencies<'msg> = { wrapper: Message -> 'msg; saveGame: string * Set<uint> -> 'msg }

    type Sate = private State of InnerState

    and private InnerState =
        | Initializing
        | Ready of UniverseReady

    and private UniverseReady = {
        dimensions: UniverseDimensions
        mode: GameMode
        universe: Universe
        previousX: float
        previousY: float
    }

    and GameMode =
        | Playing
        | Editing
        | Saving of string

    and UniverseDimensions = { minX: float; minY: float; height: int; width: int }

    let init _ =
        Initializing |> State, Cmd.ofMsg (GetUniverseSize 0)

    let setCells cells =
        function
        | State(Ready({ mode = Editing } as readyState)) ->
            { readyState with universe = { defaultUniverse with cells = cells } }
            |> Ready
            |> State
        | state -> state

    let private setDimensions height width =
        function
        | Initializing ->
            {
                dimensions = { minX = 0; minY = 0; height = height; width = width }
                mode = Editing
                universe = defaultUniverse
                previousX = 0
                previousY = 0
            }
            |> Ready
        | Ready r ->
            { r with dimensions = { r.dimensions with height = height; width = width } }
            |> Ready

    let update { wrapper = wrapper; saveGame = saveGame } (msg: Message) (State innerState: Sate) : Sate * Cmd<'msg> =
        match innerState, msg with
        | Ready({ dimensions = dimensions; mode = Editing; universe = universe } as readyState), OnMouseDown me ->

            let toCellCoord shift position =
                (position - shift) / float cellSize |> int

            let x, y =
                ((toCellCoord 316.0 (me.clientX + dimensions.minX)), (toCellCoord 57.0 (me.clientY + dimensions.minY)))

            { readyState with universe = toggleCell universe x y } |> Ready |> State, Cmd.none

        | Ready({ mode = Playing } as readyState), OnMouseDown me ->
            { readyState with previousX = me.clientX; previousY = me.clientY }
            |> Ready
            |> State,
            Cmd.none

        | _, GetUniverseSize ix ->
            let elt = document.getElementById "universe"

            if elt = null && ix < 100 then
                State innerState,
                Cmd.OfAsync.perform (fun _ -> Async.Sleep 100) () (fun _ -> ix + 1 |> GetUniverseSize |> wrapper)
            else
                State innerState,
                (int elt.offsetHeight, int elt.offsetWidth)
                |> GotUniverseSize
                |> wrapper
                |> Cmd.ofMsg

        | _, GotUniverseSize(height, width) -> innerState |> setDimensions (height - 39) width |> State, Cmd.none

        | Ready({ mode = Editing } as readyState), ToggleGameMode ->
            { readyState with mode = Playing } |> Ready |> State, Step |> wrapper |> Cmd.ofMsg

        | Ready({ mode = Playing } as readyState), ToggleGameMode _ ->
            { readyState with mode = Editing } |> Ready |> State, Cmd.none

        | Ready({ mode = Playing; universe = universe } as readyState), Step _ ->
            { readyState with universe = step universe } |> Ready |> State,
            Cmd.OfAsync.perform (fun _ -> Async.Sleep 100) () (fun _ -> wrapper Step)

        | Ready _, OnResize -> State innerState, Cmd.ofMsg (GetUniverseSize 0 |> wrapper)

        | Ready state, OnKeyDown kd when kd.key.ToLower() = "a" && state.dimensions.minX > 0 ->
            {
                state with
                    dimensions = { state.dimensions with minX = state.dimensions.minX - 10.0 }
            }
            |> Ready
            |> State,
            Cmd.none

        | Ready state, OnKeyDown kd when kd.key.ToLower() = "d" ->
            {
                state with
                    dimensions = { state.dimensions with minX = state.dimensions.minX + 10.0 }
            }
            |> Ready
            |> State,
            Cmd.none

        | Ready state, OnKeyDown kd when kd.key.ToLower() = "w" && state.dimensions.minY > 0 ->
            {
                state with
                    dimensions = { state.dimensions with minY = state.dimensions.minY - 10.0 }
            }
            |> Ready
            |> State,
            Cmd.none

        | Ready state, OnKeyDown kd when kd.key.ToLower() = "s" ->
            {
                state with
                    dimensions = { state.dimensions with minY = state.dimensions.minY + 10.0 }
            }
            |> Ready
            |> State,
            Cmd.none

        | Ready({ mode = Editing } as state), SaveGame ->
            {
                state with
                    mode = Saving $"""Save of {DateTime.Now.ToString("MM/dd/yy H:mm:ss")}"""
            }
            |> Ready
            |> State,
            Cmd.none

        | Ready({ mode = Saving _ } as state), CancelSaveGame ->
            { state with mode = Editing } |> Ready |> State, Cmd.none

        | Ready({ mode = Saving name; universe = { cells = cells } } as state), ProceedSaveGame ->
            { state with mode = Editing } |> Ready |> State, (name, cells) |> saveGame |> Cmd.ofMsg

        | Ready({ mode = Saving _ } as state), SaveGameNameChange str ->
            { state with mode = Saving str } |> Ready |> State, Cmd.none

        | _ -> State innerState, Cmd.none

    let private renderCell ix =
        let x, y = toCoords ix

        Svg.rect [
            svg.x (x * cellSize)
            svg.y (y * cellSize)
            svg.height cellSize
            svg.width cellSize
        ]

    let renderNavBar dispatch =
        function
        | Playing ->
            Html.button [
                prop.classes [ "btn" ]
                prop.onClick (fun _ -> dispatch ToggleGameMode)
                prop.children [
                    Html.i [
                        prop.classes [ "fa-solid"; "fa-stop" ]
                    ]
                ]
            ]
            |> List.singleton

        | Editing -> [
            Html.button [
                prop.classes [ "btn" ]
                prop.onClick (fun _ -> dispatch SaveGame)
                prop.children [
                    Html.i [
                        prop.classes [
                            "fa-solid"
                            "fa-floppy-disk"
                        ]
                    ]
                ]
            ]
            Html.button [
                prop.classes [ "btn" ]
                prop.onClick (fun _ -> dispatch ToggleGameMode)
                prop.children [
                    Html.i [
                        prop.classes [ "fa-solid"; "fa-play" ]
                    ]
                ]
            ]
          ]
        | Saving str -> [
            Html.input [
                prop.type' "text"
                prop.classes [ "input-text" ]
                prop.value str
                prop.onChange (SaveGameNameChange >> dispatch)
            ]
            Html.button [
                prop.classes [ "btn" ]
                prop.onClick (fun _ -> dispatch ProceedSaveGame)
                prop.children [
                    Html.i [
                        prop.classes [ "fa-solid"; "fa-check" ]
                    ]
                ]
            ]
            Html.button [
                prop.classes [ "btn" ]
                prop.onClick (fun _ -> dispatch CancelSaveGame)
                prop.children [
                    Html.i [
                        prop.classes [ "fa-solid"; "fa-xmark" ]
                    ]
                ]
            ]
          ]

    let render (dispatch: Message -> unit) (State innerState) =
        Html.div [
            prop.id "universe"
            prop.classes [ "card game" ]
            prop.children [
                match innerState with
                | Initializing ->
                    Html.div [
                        prop.className "center-screen"
                        prop.text "Loading..."
                    ]
                | Ready { dimensions = dimensions; universe = universe; mode = mode } ->
                    Html.div [
                        prop.classes [ "nav-bar" ]
                        prop.children (renderNavBar dispatch mode)
                    ]

                    Html.div [
                        prop.children [
                            Svg.svg [
                                svg.viewBox (
                                    int dimensions.minX,
                                    int dimensions.minY,
                                    dimensions.width,
                                    dimensions.height
                                )
                                svg.onMouseUp (dispatch << OnMouseUp)
                                svg.onMouseDown (dispatch << OnMouseDown)
                                svg.children (universe.cells |> Set.toList |> List.map renderCell)
                            ]

                        ]
                    ]
            ]
        ]

    let unwrapUniverse =
        function
        | State(Ready { universe = universe }) -> Some universe
        | _ -> None

    let resizeSub onKeydown =
        fun dispatch ->
            let act _ = dispatch onKeydown

            window.addEventListener ("resize", act)

            { new IDisposable with
                member _.Dispose() = window.addEventListener ("resize", act)
            }

    let keydownSub onKeyDown =
        fun dispatch ->
            let act (e: Event) =
                match e with
                | :? KeyboardEvent as ke -> onKeyDown ke |> dispatch
                | _ -> Noop |> dispatch

            window.addEventListener ("keydown", act)

            { new IDisposable with
                member _.Dispose() =
                    window.addEventListener ("keydown", act)
            }

    let subscribe (State _) = [
        [ "resize" ], resizeSub OnResize
        [ "keydown" ], keydownSub OnKeyDown
    ]
