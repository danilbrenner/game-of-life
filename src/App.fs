module GameOfLife

open Observables

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

let universe = [| false |]

type Offset = { Top:float; Left:float }

let indexToCoords ix =
    let x = ix / 71
    let y = ix - (x*71)
    x, y

let coordsToIndex (x, y) = 
    let xi = int (x/14.)
    let yi = int (y/14.) * 71
    xi+yi

let drawUnverse (ctx:CanvasRenderingContext2D) (universe:bool array) =
    ctx.fillStyle <- !^"#009933"
    ctx.fillRect (0., 0., 995., 600.)
    universe
    |> Array.iteri (fun i v -> 
        match v with
        | false -> ()
        | true ->
            let x, y = indexToCoords i
            let positionTop = ((float x)*14.)+1.
            let positionLeft = ((float y)*14.)+1.
            ctx.fillStyle <- !^"#fff"
            ctx.fillRect (positionLeft, positionTop, 12., 12.))

let mouseMove offset (subj:Subject<int>) e = 
    unbox<MouseEvent> e
    |> (fun ev -> ev.clientX - offset.Left, ev.clientY - offset.Top)
    |> coordsToIndex
    |> subj.Next
    
let test () =
    printfn "Lolo"

let init() =
    let canvas = document.getElementsByTagName_canvas().[0]
    let button = document.getElementById("start-button")
    button.onclick <- unbox(test)
    
    canvas.width <- 995.
    canvas.height <- 800.

    let ctx = canvas.getContext_2d()
    let drawUni = drawUnverse ctx
    
    
    let universeArray = Array.zeroCreate<bool> 4047
    
    let subject = Subject<int>()
    let mv = mouseMove {Top = canvas.offsetTop; Left = canvas.offsetLeft } subject 
    let mutable isMouseDown = false
    let mutable curItem = -1

    canvas.addEventListener("mousedown", unbox(fun e -> 
        isMouseDown <- true
        mv e))
    canvas.addEventListener("mouseup", unbox(fun e -> 
        isMouseDown <- false
        curItem <- -1))
    
    canvas.addEventListener("mousemove", unbox(mv))    

    let subs = 
        subject
        |> Observable.filter (fun i -> isMouseDown)        
        |> Observable.filter (fun i -> i <> curItem)
        |> Observable.subscribe (fun i -> 
            curItem <- i
            universeArray.[i] <- not universeArray.[i]
            drawUni universeArray)
    
    drawUni universeArray    
    printfn "Init completed."
    
init()