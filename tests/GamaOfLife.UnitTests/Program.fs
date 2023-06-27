open Expecto
open GameOfLife.Model

let tests =
    test "A simple test" {
        let subject = Say.hello "U"
        Expect.equal subject "Hello U" "The strings should equal"
    }

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args tests
