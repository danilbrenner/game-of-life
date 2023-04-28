namespace GameOfLife.Tests

open Expecto

module Run =
    [<EntryPoint>]
    let main argv =
        Tests.runTestsInAssembly defaultConfig argv
