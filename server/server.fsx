#r "packages/Suave/lib/net40/Suave.dll"

open System.IO
open Suave
open Suave.Filters
open Suave.Operators

let app : WebPart =
  choose [
    GET >=> choose [ path "/" >=> Redirection.redirect "/index.html"; Files.browseHome ]
    // GET >=> Files.browseHome
    RequestErrors.NOT_FOUND "Page not found." 
  ]

let config =
  { defaultConfig with homeFolder = Some (Path.GetFullPath "../public") }

startWebServer config app