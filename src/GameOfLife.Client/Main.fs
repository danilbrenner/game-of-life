module GameOfLife.Client.Main

open System
open System.Net.Http
open System.Net.Http.Json
open GameOfLife.Client.Pages
open Microsoft.AspNetCore.Components
open Elmish
open Bolero
open Bolero.Html

type Main = Template<"wwwroot/main.html">
type Info = Template<"wwwroot/pages/info.html">


type Page =
    | [<EndPoint "/game">] HomePage of Home.State
    | [<EndPoint "/info">] InfoPage

type Model = { page: Page }

type Message = SetPage of Page

let router = Router.infer SetPage (fun model -> model.page)
let initModel = { page = Home.init |> HomePage }

let update (http: HttpClient) message model =
    match message with
    | SetPage page -> { model with page = page }, Cmd.none

let view model dispatch =
    Main()
        .Body(
            cond model.page
            <| function
                | HomePage state -> Home.view state
                | InfoPage -> Info.Info().Elt()
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val HttpClient = Unchecked.defaultof<HttpClient> with get, set

    override this.Program =
        let update = update this.HttpClient

        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
