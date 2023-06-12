module Test.Generated.Main exposing (main)

import Example

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 124933068283697
        , processes = 4
        , globs =
            []
        , paths =
            [ "/home/felix/Dev/room-auction/tests/Example.elm"
            ]
        }
        [ ( "Example"
          , [ Test.Runner.Node.check Example.suite
            , Test.Runner.Node.check Example.declinedTest
            , Test.Runner.Node.check Example.nextUpTest
            , Test.Runner.Node.check Example.nextDownTest
            ]
          )
        ]