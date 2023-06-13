module MainTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (BidStatus(..), declineSort, declinedSortHelper, nextState)
import Test exposing (..)


nextStateTest : Test
nextStateTest =
    let
        oldState =
            { max = 400
            , currBidder = { name = "steve", status = Active }
            , nextBidder =
                [ { name = "mary", status = Active }
                , { name = "rosebud", status = Declined 200 }
                ]
            , currPrice = 350
            }

        want =
            { max = 400
            , currBidder = { name = "mary", status = Active }
            , nextBidder =
                [ { name = "steve", status = Active }
                , { name = "rosebud", status = Declined 200 }
                ]
            , currPrice = 350
            }
    in
    test
        "correctly rearrange bidders!"
        (\_ -> Expect.equal want (nextState oldState))


declineSortHelperTest : Test
declineSortHelperTest =
    let
        a =
            { name = "mary", status = Declined 400 }

        b =
            { name = "steve", status = Active }
    in
    test "should return correct Order value" (\_ -> Expect.equal GT (declinedSortHelper a b))


declineSortTest : Test
declineSortTest =
    let
        input =
            [ { name = "steve", status = Active }
            , { name = "mary", status = Declined 4500 }
            , { name = "bruce", status = Active }
            ]

        want =
            [ { name = "steve", status = Active }
            , { name = "bruce", status = Active }
            , { name = "mary", status = Declined 4500 }
            ]
    in
    test "declined should all be at the end!" (\_ -> Expect.equal want (declineSort input))
