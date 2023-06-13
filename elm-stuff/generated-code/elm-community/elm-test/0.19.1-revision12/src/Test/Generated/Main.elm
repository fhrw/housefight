module Test.Generated.Main exposing (main)

import Example
import MainTests

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 124229188103743
        , processes = 4
        , globs =
            []
        , paths =
            [ "/home/felix/Dev/room-auction/tests/Example.elm"
            , "/home/felix/Dev/room-auction/tests/MainTests.elm"
            ]
        }
        [ ( "Example"
          , [ Test.Runner.Node.check Example.suite
            ]
          )
        , ( "MainTests"
          , [ Test.Runner.Node.check MainTests.nextStateTest
            , Test.Runner.Node.check MainTests.declineSortHelperTest
            , Test.Runner.Node.check MainTests.declineSortTest
            ]
          )
        ]