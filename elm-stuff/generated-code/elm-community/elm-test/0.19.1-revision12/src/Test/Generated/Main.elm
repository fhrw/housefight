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
        , report = ConsoleReport Monochrome
        , seed = 56375207770298
        , processes = 10
        , globs =
            []
        , paths =
            [ "/Users/felix/Dev/housefight/tests/Example.elm"
            , "/Users/felix/Dev/housefight/tests/MainTests.elm"
            ]
        }
        [ ( "Example"
          , [ Test.Runner.Node.check Example.suite
            ]
          )
        , ( "MainTests"
          , [ Test.Runner.Node.check MainTests.nextAuctionStateTest
            , Test.Runner.Node.check MainTests.hasWinnerTest
            , Test.Runner.Node.check MainTests.nextStateTest
            , Test.Runner.Node.check MainTests.declineSortHelperTest
            , Test.Runner.Node.check MainTests.declineSortTest
            ]
          )
        ]