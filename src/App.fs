module GameOfLife

open Observables
open Commons
open Canvas
open Game

open Fable.Import.Browser

type AppState(size) = 
    let mutable universe = Array.zeroCreate<bool> size
    let mutable interval:float option = None
    
    member this.SetUniverse uni =
        universe <- uni
    member this.GetUniverse () =
        universe
    member this.ResetUniverse () =
        universe <- Array.zeroCreate<bool> size
        universe
    member this.SetInterval int =
        interval <- int
    member this.GetInterval () =
        interval

let setupButton id action =
    document.getElementById(id)
    |> tee (fun b -> b.onclick <- unbox(action))

let toggleRun btnId =
    function
    | None -> 
        document.getElementById(btnId).innerHTML <- """<i class="fa fa-play" aria-hidden="true"></i>"""            
    | Some i -> 
        document.getElementById(btnId).innerHTML <- """<i class="fa fa-stop" aria-hidden="true"></i>"""

let changeUniverse (state:AppState) (drawUniSubj:Subject<bool array>) action =
    state.GetUniverse()
    |> action
    |> tee state.SetUniverse
    |> drawUniSubj.Next

let init() =
    
    let state = AppState (4047)
    let canvas = document.getElementsByTagName_canvas().[0]
    let drawUniverseSubj = Subject<bool array>()
    let stepIt () = changeUniverse state drawUniverseSubj step    

    let togglePlay () =
        state.GetInterval ()
        |> switch
            (fun i -> 
                window.clearInterval(i)
                None)
            (fun () -> Some (window.setInterval(stepIt, 200.)))
        |> tee (fun i -> toggleRun "start-button" i)
        |> state.SetInterval
        ()

    let toggleCellSubj = 
        document.getElementsByTagName_canvas().[0]
        |> setupCanvas { Height = 995.; Width = 602.; CellsInLine = 71 } drawUniverseSubj
    
    drawUniverseSubj.Next <| state.GetUniverse()
    
    toggleCellSubj
    |> Observable.filter (fun i -> state.GetInterval() = None)
    |> Observable.subscribe (fun i ->
        changeUniverse state drawUniverseSubj <| Array.mapi (fun ix v -> if i = ix then (not v) else v))
    |> ignore
            
    setupButton "step-button" stepIt
    |> ignore

    setupButton "start-button" togglePlay
    |> ignore
    
    setupButton "clear-button" (fun () -> 
        if state.GetInterval() = None then 
            state.ResetUniverse ()
            |> drawUniverseSubj.Next)
    |> ignore

    printfn "Init completed. Now"
    
init()