namespace GamaOfLife.UnitTests

[<AutoOpen>]
module CoordinatesGen =

    open Expecto
    open FsCheck

    type ValidCoordinate = ValidCoordinate of int

    type InvalidCoordinate = InvalidCoordinate of int

    type ValidCoordinatesGenerator() =
        static member ValidCoordinate() =
            { new Arbitrary<ValidCoordinate>() with
                override x.Generator =
                    gen {
                        let! n = Gen.choose (0, 65535)
                        return ValidCoordinate n
                    }
            }

    type InvalidCoordinatesGenerator() =
        static member InvalidCoordinate() =
            { new Arbitrary<InvalidCoordinate>() with
                override x.Generator =
                    gen {
                        let! n = Arb.generate<int> |> Gen.filter (fun i -> i < 0 || i > 65535)
                        return InvalidCoordinate n
                    }
            }

    let coordinatesConfig = {
        FsCheckConfig.defaultConfig with
            arbitrary = [
                typeof<ValidCoordinatesGenerator>
                typeof<InvalidCoordinatesGenerator>
            ]
    }
