namespace GamaOfLife.UnitTests

module IsAliveTests =

    open Expecto
    open GameOfLife.Model.Game

    [<Tests>]
    let isAliveTests =
        testList "Is alive" [
            testPropertyWithConfig coordinatesConfig "When invalid coordinates"
            <| fun (InvalidCoordinate x) (InvalidCoordinate y) universe ->
                let result = isCellAlive x y universe
                Expect.isFalse result "Should return false"

            testPropertyWithConfig coordinatesConfig "When requested cell is alive"
            <| fun cells ->
                let result =
                    {
                        cells = cells |> Set.add 327690u
                        generations = 0
                    }
                    |> isCellAlive 5 10

                Expect.isTrue result "Should return true"

            testPropertyWithConfig coordinatesConfig "When requested cell is dead"
            <| fun universe ->
                let result =
                    {
                        cells = universe |> Set.remove 327690u
                        generations = 0
                    }
                    |> isCellAlive 5 10

                Expect.isFalse result "Should return false"
        ]
