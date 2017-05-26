module Observables

open System
open System.Collections.Generic

let private getIndex lst item = List.tryFindIndex (fun i -> i = item) lst

let private split ix ls =
  let rec inner vla = 
    match vla with
        | (0, fa, la) | (_, fa, ([] as la)) -> (List.rev fa), la
        | (n, fa, x::la) -> inner ((n-1), (x::fa), la)
  inner (ix, [], ls)

let private removeIx lst ix = 
    let h, t = split ix lst
    match t with
    | rh::rt -> h@rt
    | rt -> rt

let private removeItem lst observer =
        let ix = getIndex lst observer
        match ix with
        | Some i -> removeIx lst i
        | None -> lst

type Subject<'T> () =
    let mutable stopped = false
    let mutable observers: IObserver<'T> list = []
    let iter = (fun f -> observers |> List.iter f)

    member x.Compleet () =
        if not stopped then
            stopped <- true
            iter (fun observer -> observer.OnCompleted())
    member x.Error ex =
        if not stopped then
            stopped <- true
            iter (fun observer -> observer.OnError(ex))
    member x.Next value =
        if not stopped then
            iter (fun observer -> observer.OnNext(value))
    interface IObservable<'T> with
        member this.Subscribe(observer:IObserver<'T>) =
            observers <- observer::observers
            { new IDisposable with
                member this.Dispose() = observers <- removeItem observers observer
            }