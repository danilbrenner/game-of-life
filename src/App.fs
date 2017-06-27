module GameOfLife

open Observables
open Game

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

type Offset = { Top:float; Left:float }
type UniverseSizeConstants = { Height: float; Width: float; CellsInLine: int; }

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

let indexToCoords size ix =
    let x = ix / size.CellsInLine
    let y = ix - (x*size.CellsInLine)
    x, y

let coordsToIndex size (x, y) = 
    let xi = int (x/14.)
    let yi = int (y/14.) * size.CellsInLine
    xi+yi

let drawUnverse size (ctx:CanvasRenderingContext2D) (universe:bool array) =
    ctx.fillStyle <- !^"#009933"
    ctx.fillRect (0., 0., size.Height, size.Width)
    universe
    |> Array.iteri (fun i v -> 
        match v with
        | false -> ()
        | true ->
            let x, y = indexToCoords size i
            let positionTop = ((float x)*14.)+1.
            let positionLeft = ((float y)*14.)+1.
            ctx.fillStyle <- !^"#fff"
            ctx.fillRect (positionLeft, positionTop, 12., 12.))

let mouseMove size getOffset (subj:Subject<int>) e = 
    let offset = getOffset()
    unbox<MouseEvent> e
    |> (fun ev -> ev.clientX - offset.Left, ev.clientY - offset.Top)
    |> coordsToIndex size
    |> subj.Next

let tee f x = 
    f x |> ignore
    x

let setupCanvas size =
    document.getElementsByTagName_canvas().[0]
    |> tee (fun c -> c.width <- size.Height)
    |> tee (fun c -> c.height <- size.Width)
    |> (fun c-> drawUnverse size <| c.getContext_2d(), c)

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

let subscribe callback subj =
    let mutable curItem = -1        
    Some
        (subj
        |> Observable.filter (fun i -> i <> curItem)
        |> Observable.subscribe (fun i -> 
                curItem <- i
                callback i))

let init() =
    
    let state = AppState (4047)
    let size = { Height = 995.; Width = 602.; CellsInLine = 71 }

    let (drawUni, canvas) = setupCanvas size
    
    let stepIt () = 
        state.GetUniverse()
        |> step 
        |> tee drawUni
        |> state.SetUniverse
    
    setupButton "step-button" stepIt    
    |> ignore

    "start-button"
    |> (fun i -> setupButton i (fun () -> toggleRun i state.SetInterval stepIt (state.GetInterval())))
    |> ignore

    //------- Setup universe
    
    let subject = Subject<int>()
    let mv = mouseMove size (fun () -> {Top = canvas.offsetTop; Left = canvas.offsetLeft }) subject 
    let mutable subs:IDisposable option = None
    
    let toggleItem i =
        state.GetUniverse()
        |> Array.mapi (fun ix v -> if i = ix then (not v) else v)
        |> tee state.SetUniverse
        |> drawUni

    canvas.addEventListener("mousedown", unbox(fun e -> 
        match state.GetInterval() with
        | None -> subs <- subscribe toggleItem subject
        | Some _ -> ()
        mv e))
    
    canvas.addEventListener("mouseup", unbox(fun e -> 
        match subs with
        | Some ds -> 
            ds.Dispose()
            subs <- None
        | None -> ()))
    
    canvas.addEventListener("mousemove", unbox(mv))
    state.GetUniverse()
    |> drawUni
    printfn "Init completed. Now"
    
init()