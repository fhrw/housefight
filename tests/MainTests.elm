module MainTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (BidStatus(..), declineSort, declinedSortHelper)
import Test exposing (..)


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
