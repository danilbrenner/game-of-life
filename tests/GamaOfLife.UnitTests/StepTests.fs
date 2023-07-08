namespace GamaOfLife.UnitTests

module StepTests =

    open Expecto
    open FsCheck
    open GameOfLife.Model.Game

    [<Tests>]
    let gameTests =
        let lessThenTwoNeighbours = [
            Set.empty
            Set.empty |> Set.add 262153u
        ]

        let twoOrThreeNeighbours = [
            Set.empty |> Set.add 262153u |> Set.add 327689u
            Set.empty |> Set.add 393225u |> Set.add 393226u |> Set.add 262154u
        ]

        let moreThenThreeNeighbours = [
            Set.empty
            |> Set.add 262153u
            |> Set.add 327689u
            |> Set.add 262154u
            |> Set.add 393227u
            Set.empty
            |> Set.add 262153u
            |> Set.add 327689u
            |> Set.add 393227u
            |> Set.add 393225u
            |> Set.add 393226u
            Set.empty
            |> Set.add 262153u
            |> Set.add 327689u
            |> Set.add 262154u
            |> Set.add 393227u
            |> Set.add 393225u
            |> Set.add 262155u
            |> Set.add 327691u
            |> Set.add 393226u
        ]

        let otherThenThreeNeighbours = [
            Set.empty
            Set.empty |> Set.add 262153u
            Set.empty |> Set.add 262153u |> Set.add 327689u
            Set.empty
            |> Set.add 262153u
            |> Set.add 327689u
            |> Set.add 262154u
            |> Set.add 393227u
            Set.empty
            |> Set.add 262153u
            |> Set.add 327689u
            |> Set.add 393227u
            |> Set.add 393225u
            |> Set.add 393226u
            Set.empty
            |> Set.add 262153u
            |> Set.add 327689u
            |> Set.add 262154u
            |> Set.add 393227u
            |> Set.add 393225u
            |> Set.add 262155u
            |> Set.add 327691u
            |> Set.add 393226u
        ]

        testList "Game step" [
            testTheory "Live cell with fewer than two live neighbours" lessThenTwoNeighbours
            <| fun cells ->
                let result =
                    {
                        generations = 0
                        cells = cells |> Set.add 327690u
                    }
                    |> step

                Expect.isFalse (isCellAlive 5 10 result) "Should die"
                Expect.equal result.generations 1 " Generations should increment"

            testTheory "Live cell with two or three live neighbours" twoOrThreeNeighbours
            <| fun cells ->
                let result =
                    {
                        generations = 3
                        cells = cells |> Set.add 327690u
                    }
                    |> step

                Expect.isTrue (isCellAlive 5 10 result) "Should live on"
                Expect.equal result.generations 4 " Generations should increment"

            testTheory "Live cell with more than three live neighbours" moreThenThreeNeighbours
            <| fun cells ->
                let result =
                    {
                        generations = 4
                        cells = cells |> Set.add 327690u
                    }
                    |> step

                Expect.isFalse (isCellAlive 5 10 result) "Should die"
                Expect.equal result.generations 5 " Generations should increment"


            test "Dead cell with exactly three live neighbours" {
                let result =
                    {
                        generations = 5
                        cells = Set.empty |> Set.add 262153u |> Set.add 327689u |> Set.add 262154u
                    }
                    |> step

                Expect.isTrue (isCellAlive 5 10 result) "Should become a live"
                Expect.equal result.generations 6 " Generations should increment"
            }

            testTheory "Dead cell with not three live neighbours" otherThenThreeNeighbours
            <| fun cells ->
                let result =
                    {
                        generations = 6
                        cells = cells
                    }
                    |> step

                Expect.isFalse (isCellAlive 5 10 result) "Stay dead"
                Expect.equal result.generations 7 " Generations should increment"
        ]
