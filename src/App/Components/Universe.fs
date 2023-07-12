namespace GameOfLife.Components

open System
open Browser.Types
open Elmish
open Feliz
open GameOfLife.Model.Game
open Microsoft.FSharp.Core
open Browser

module Universe =

    [<Literal>]
    let private cellSize: int = 10

    type Msg =
        | OnMouseDown of MouseEvent
        | OnMouseUp of MouseEvent
        | OnMouseMove of MouseEvent
        | GetUniverseSize of int
        | GotUniverseSize of int * int
        | ToggleGameMode
        | Step
        | OnResize

    type Sate = private State of InnerState

    and private InnerState =
        | Initializing
        | Ready of UniverseReady

    and private UniverseReady = {
        dimensions: UniverseDimensions
        mode: GameMode
        universe: Universe
        isNavigating: bool
        previousX: float
        previousY: float
    }

    and GameMode =
        | Playing
        | Editing

    and UniverseDimensions = { minX: float; minY: float; height: int; width: int }

    let init _ =
        Initializing |> State, Cmd.ofMsg (GetUniverseSize 0)

    let private setDimensions height width =
        function
        | Initializing ->
            {
                dimensions = { minX = 0; minY = 0; height = height; width = width }
                mode = Editing
                universe = defaultUniverse
                isNavigating = false
                previousX = 0
                previousY = 0
            }
            |> Ready
        | Ready r ->
            { r with dimensions = { r.dimensions with height = height; width = width } }
            |> Ready

    let update msg (State innerState) =
        match innerState, msg with
        | Ready({ dimensions = dimensions; mode = Editing; universe = universe } as readyState), OnMouseDown me ->
            let toCellCoord shift position =
                (position - shift) / float cellSize |> int

            let x, y =
                ((toCellCoord 320.0 (me.clientX + dimensions.minX)), (toCellCoord 11.0 (me.clientY + dimensions.minY)))

            { readyState with universe = toggleCell universe x y } |> Ready |> State, Cmd.none

        | Ready({ mode = Playing } as readyState), OnMouseDown me ->
            {
                readyState with
                    isNavigating = true
                    previousX = me.clientX
                    previousY = me.clientY
            }
            |> Ready
            |> State,
            Cmd.none

        | _, OnMouseDown _ -> State innerState, Cmd.none

        | Ready({ mode = Playing } as readyState), OnMouseUp _ ->
            { readyState with isNavigating = false } |> Ready |> State, Cmd.none

        | _, OnMouseUp _ -> State innerState, Cmd.none

        | Ready({ dimensions = dimensions; isNavigating = true } as readyState), OnMouseMove me ->

            let newMinX, newMinY =
                dimensions.minX + readyState.previousX - me.clientX, dimensions.minY + readyState.previousY - me.clientY

            {
                readyState with
                    dimensions = {
                        dimensions with
                            minX = if newMinX > 0 then newMinX else 0
                            minY = if newMinY > 0 then newMinY else 0
                    }
                    previousX = me.clientX
                    previousY = me.clientY
            }
            |> Ready
            |> State,
            Cmd.none

        | _, OnMouseMove _ -> State innerState, Cmd.none

        | _, GetUniverseSize ix ->
            let elt = document.getElementById "universe"

            if elt = null && ix < 100 then
                State innerState, Cmd.OfAsync.perform (fun _ -> Async.Sleep 100) () (fun _ -> GetUniverseSize <| ix + 1)
            else
                State innerState, (int elt.offsetHeight, int elt.offsetWidth) |> GotUniverseSize |> Cmd.ofMsg

        | _, GotUniverseSize(height, width) -> innerState |> setDimensions height width |> State, Cmd.none

        | Ready({ mode = Editing } as readyState), ToggleGameMode ->
            { readyState with mode = Playing } |> Ready |> State, Cmd.ofMsg Step

        | Ready({ mode = Playing } as readyState), ToggleGameMode _ ->
            { readyState with mode = Editing } |> Ready |> State, Cmd.none

        | Initializing, ToggleGameMode -> State innerState, Cmd.none

        | Ready({ mode = Playing; universe = universe } as readyState), Step _ ->
            { readyState with universe = step universe } |> Ready |> State,
            Cmd.OfAsync.perform (fun _ -> Async.Sleep 100) () (fun _ -> Step)

        | _, Step -> State innerState, Cmd.none

        | Ready _, OnResize -> State innerState, Cmd.ofMsg (GetUniverseSize 0)
        | Initializing, OnResize -> State innerState, Cmd.none

    let private renderCell ix =
        let x, y = toCoords ix

        Svg.rect [
            svg.x (x * cellSize)
            svg.y (y * cellSize)
            svg.height (cellSize - 1)
            svg.width (cellSize - 1)
        ]

    let render (dispatch: Msg -> unit) (State innerState) =
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
                | Ready { dimensions = dimensions; universe = universe } ->
                    Svg.svg [
                        svg.viewBox (int dimensions.minX, int dimensions.minY, dimensions.width, dimensions.height)

                        svg.onMouseUp (dispatch << OnMouseUp)
                        svg.onMouseDown (dispatch << OnMouseDown)
                        svg.onMouseMove (dispatch << OnMouseMove)
                        svg.children (universe.cells |> Set.toList |> List.map renderCell)
                    ]
            ]
        ]

    let unwrapUniverse =
        function
        | State(Ready { universe = universe }) -> Some universe
        | _ -> None

    let isPlaying =
        function
        | State(Ready { mode = Playing }) -> true
        | _ -> false

    let resizeSub onResize =
        fun dispatch ->
            let act _ = dispatch onResize

            window.addEventListener ("resize", act)

            { new IDisposable with
                member _.Dispose() = window.addEventListener ("resize", act)
            }

    let subscribe (State _) = [
        [ "resize" ], resizeSub OnResize
    ]
