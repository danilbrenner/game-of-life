namespace GameOfLife

open System
open Fable.SimpleJson
open GameOfLife.Model
open GameOfLife.Model.User
open Browser

module Data =
    let saveSettings (settings: Settings) =
        settings
        |> Json.stringify
        |> (fun s -> window.localStorage.setItem ("user-settings", s))

    let getSettings () =
        window.localStorage.getItem "user-settings" |> Json.tryParseAs<Settings>
        <?> defaultSettings

    let getSavedGames () : (Guid * string) list =
        window.localStorage.getItem "games-list"
        |> Json.tryParseAs<(Guid * string) list>
        <?> []

    let getSavedGame (id: Guid) =
        window.localStorage.getItem (id.ToString()) |> Json.tryParseAs<Set<uint>>
        <?> Set.empty

    let saveGame (name: string) cells =
        let newGameId = Guid.NewGuid()
        let games = (newGameId, name) :: getSavedGames ()
        window.localStorage.setItem ("games-list", games |> Json.stringify)
        window.localStorage.setItem (newGameId.ToString(), cells |> Json.stringify)
        games

    let removeGame (id: Guid) =
        let games =
            getSavedGames () |> List.filter (fst >> ((=) id) >> not) |> Json.stringify

        window.localStorage.setItem ("games-list", games)
        window.localStorage.removeItem <| id.ToString()
        getSavedGames ()
