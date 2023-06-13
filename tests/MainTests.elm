module MainTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (BidStatus(..), Direction(..), declineSort, declinedSortHelper, hasWinner, nextAuctionState, nextBidders)
import Test exposing (..)


nextAuctionStateTest : Test
nextAuctionStateTest =
    let
        state =
            { max = 400
            , currBidder = { name = "steve", status = Active }
            , nextBidder =
                [ { name = "mary", status = Active }
                , { name = "rosebud", status = Declined 400 }
                ]
            , currPrice = 400
            , step = 200
            }

        new =
            { max = 400
            , currBidder = { name = "mary", status = Active }
            , nextBidder =
                [ { name = "steve", status = Active }
                , { name = "rosebud", status = Declined 400 }
                ]
            , currPrice = 200
            , step = 100
            }
    in
    test "outputs correct next step" (\_ -> Expect.equal new (nextAuctionState state Down))


hasWinnerTest : Test
hasWinnerTest =
    let
        state =
            { max = 400
            , currBidder = { name = "steve", status = Active }
            , nextBidder =
                [ { name = "mary", status = Active }
                , { name = "rosebud", status = Declined 200 }
                ]
            , currPrice = 350
            , step = 50
            }
    in
    test "doesn't have a winner" (\_ -> Expect.equal False (hasWinner state))


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
            , step = 50
            }

        want =
            { max = 400
            , currBidder = { name = "mary", status = Active }
            , nextBidder =
                [ { name = "steve", status = Active }
                , { name = "rosebud", status = Declined 200 }
                ]
            , currPrice = 350
            , step = 50
            }
    in
    test
        "correctly rearrange bidders!"
        (\_ -> Expect.equal want (nextBidders oldState))


declineSortHelperTest : Test
declineSortHelperTest =
    let
        a =
            { name = "mary", status = Declined 400 }

        b =
            { name = "steve", status = Active }
    in
    test "should return correct Order value"
        (\_ ->
            Expect.equal
                GT
                (declinedSortHelper a b)
        )


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
