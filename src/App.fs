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
    
    member this.SetInterval int =
        interval <- int
    member this.GetInterval () =
        interval

let setupButton id action =
    document.getElementById(id)
    |> tee (fun b -> b.onclick <- unbox(action))

let toggleRun btnId callback stepIt =
    function
    | None -> 
        document.getElementById(btnId).innerHTML <- """<i class="fa fa-stop" aria-hidden="true"></i>"""            
        let interval = Some (window.setInterval(stepIt, 200.))
        callback interval
    | Some i -> 
        window.clearInterval(i)
        document.getElementById(btnId).innerHTML <- """<i class="fa fa-play" aria-hidden="true"></i>"""
        callback None 

let init() =
    
    let state = AppState (4047)
    let canvas = document.getElementsByTagName_canvas().[0]

    let drawUniverseSubj = Subject<bool array>()

    let toggleCellSubj = 
        document.getElementsByTagName_canvas().[0]
        |> setupCanvas { Height = 995.; Width = 602.; CellsInLine = 71 } drawUniverseSubj
    
    drawUniverseSubj.Next <| state.GetUniverse()

    let toggleItem i =
        state.GetUniverse()
        |> Array.mapi (fun ix v -> if i = ix then (not v) else v)
        |> tee state.SetUniverse
        |> drawUniverseSubj.Next
    
    toggleCellSubj
    |> Observable.filter (fun i -> state.GetInterval() = None)
    |> Observable.subscribe toggleItem
    |> ignore
    
    let stepIt () = 
        state.GetUniverse()
        |> step 
        |> tee drawUniverseSubj.Next
        |> state.SetUniverse
    
    setupButton "step-button" stepIt
    |> ignore

    "start-button"
    |> (fun i -> setupButton i (fun () -> toggleRun i state.SetInterval stepIt (state.GetInterval())))
    |> ignore
    
    setupButton "save-button" stepIt
    |> ignore


    printfn "Init completed. Now"
    
init()