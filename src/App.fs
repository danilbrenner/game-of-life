module GameOfLife

open Observables
open Game

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

//let universe = [| false |]

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
    ctx.fillRect (0., 0., 995., 602.)
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

let init() =
    let canvas = document.getElementsByTagName_canvas().[0]
    let button = document.getElementById("step-button")
    let startButton = document.getElementById("start-button")
    
    
    canvas.width <- 995.
    canvas.height <- 602.

    let ctx = canvas.getContext_2d()
    let drawUni = drawUnverse ctx
    
    let mutable universeArray = Array.zeroCreate<bool> 4047
    
    let stepIt () = 
        universeArray <- step universeArray
        drawUni universeArray
        printfn "Step"

    button.onclick <- unbox(stepIt)
    
    let mutable interval:float option = None
    let mutable canChange = true

    startButton.onclick <- unbox(fun () -> 
        match interval with
        | None -> 
            interval <- Some (window.setInterval(stepIt, 200.))
            startButton.innerHTML <- """<i class="fa fa-stop" aria-hidden="true"></i>"""
            canChange <- false   
        | Some i -> 
            window.clearInterval(i)
            interval <- None
            startButton.innerHTML <- """<i class="fa fa-play" aria-hidden="true"></i>"""
            canChange <- true            
        )
    
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
            if canChange then
                curItem <- i
                universeArray.[i] <- not universeArray.[i]
                drawUni universeArray)
    
    drawUni universeArray
    printfn "Init completed."
    
init()