namespace GamaOfLife.UnitTests

module CoordinatesTests =
    open Expecto
    open GameOfLife.Model.Game

    [<Tests>]
    let coordinatesTest =
        testList "Coordinates convertors" [
            testPropertyWithConfig coordinatesConfig "When valid coordinates"
            <| fun (ValidCoordinate x) (ValidCoordinate y) ->
                let result = fromCoords x y |> Option.map toCoords
                Expect.equal result (Some(x, y)) "Should be reversible"

            testPropertyWithConfig coordinatesConfig "When invalid coordinates"
            <| fun (InvalidCoordinate x) (InvalidCoordinate y) ->
                let result = fromCoords x y
                Expect.isNone result "Should return none"

            testPropertyWithConfig coordinatesConfig "When invalid x coordinate"
            <| fun (InvalidCoordinate x) (ValidCoordinate y) ->
                let result = fromCoords x y
                Expect.isNone result "Should return none"

            testPropertyWithConfig coordinatesConfig "When invalid y coordinate"
            <| fun (ValidCoordinate x) (InvalidCoordinate y) ->
                let result = fromCoords x y
                Expect.isNone result "Should return none"
        ]
