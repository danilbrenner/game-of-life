module Canvas

open Observables
open Commons

open System
open Fable.Core.JsInterop
open Fable.Import.Browser

type UniverseSizeConstants = { Height: float; Width: float; CellsInLine: int; }
type private Offset = { Top:float; Left:float }

let private indexToCoords size ix =
    let x = ix / size.CellsInLine
    let y = ix - (x*size.CellsInLine)
    x, y

let private coordsToIndex size (x, y) = 
    let xi = int (x/14.)
    let yi = int (y/14.) * size.CellsInLine
    xi+yi

let private drawUnverse size (ctx:CanvasRenderingContext2D) (universe:bool array) =
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

let private mouseMove size (getOffset: unit -> Offset) (subj:Subject<int>) e = 
    let offset = getOffset()
    unbox<MouseEvent> e
    |> (fun ev -> ev.clientX - offset.Left, ev.clientY - offset.Top)
    |> coordsToIndex size
    |> subj.Next

let setupCanvas size drawUniverseSubj (canvas:HTMLCanvasElement) =
    let mutable isActive = false
    let mutable curItem = -1  
    let toggleCellSubj = Subject<int>()
    let move = mouseMove size (fun () -> {Top = canvas.offsetTop; Left = canvas.offsetLeft }) toggleCellSubj

    let drawUni =
        canvas
        |> tee (fun c -> c.width <- size.Height)
        |> tee (fun c -> c.height <- size.Width)
        |> tee (fun c -> c.addEventListener("mouseout", unbox(fun e -> 
            isActive <- false)))
        |> tee (fun c -> c.addEventListener("mouseup", unbox(fun e -> 
            isActive <- false)))
        |> tee (fun c -> c.addEventListener("mousemove", unbox(move)))
        |> tee (fun c -> c.addEventListener("mousedown", unbox(fun e ->  
                isActive <- true
                curItem <- -1
                move e)))
        |> (fun c-> drawUnverse size <| c.getContext_2d())
    
    drawUniverseSubj
    |> Observable.subscribe drawUni
    |> ignore

    toggleCellSubj
    |> Observable.filter (fun i -> isActive)    
    |> Observable.filter (fun i -> i <> curItem)
    |> Observable.map (fun i ->
        curItem <- i
        i)