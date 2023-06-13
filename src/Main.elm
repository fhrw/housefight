module Main exposing (AuctionState, BidStatus(..), Direction(..), declineSort, declinedSortHelper, hasWinner, main, nextAuctionState, nextBidders)

import Browser
import Html exposing (Html, button, div, h1, h2, h3, input, p, text)
import Html.Attributes exposing (placeholder, value)
import Participant exposing (Participant)
import Room exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


type Model
    = Setup SetupFields
    | SetupFail Problem
    | Auction AuctionState
    | Results


type alias SetupFields =
    { people : List String
    , totalRent : Maybe Float
    , roomNames : List String
    }


createAuction : SetupFields -> AuctionState
createAuction setup =
    let
        { people, totalRent } =
            setup

        bidders =
            List.map nameToBidder people

        fake =
            { name = "steve", status = Active }

        defaultRent =
            500
    in
    case totalRent of
        Nothing ->
            { max = defaultRent
            , currBidder = fake
            , nextBidder = bidders
            , currPrice = defaultRent
            , step = defaultRent * 0.5
            }

        Just val ->
            { max = val
            , currBidder = fake
            , nextBidder = bidders
            , currPrice = val
            , step = val * 0.5
            }


nameToBidder : String -> Bidder
nameToBidder name =
    { name = name, status = Active }


type alias AuctionState =
    { max : Float
    , currBidder : Bidder
    , nextBidder : List Bidder
    , currPrice : Float
    , step : Float
    }


type Direction
    = Up
    | Down


type alias Bidder =
    { name : String, status : BidStatus }


type BidStatus
    = Active
    | Declined Float


type Problem
    = PeopleProblem
    | RentProblem
    | RoomProblem


nextAuctionState : AuctionState -> Direction -> AuctionState
nextAuctionState old dir =
    { old | currPrice = nextAmount old.currPrice old.step dir, step = old.step * 0.5 }
        |> nextBidders


nextBidders : AuctionState -> AuctionState
nextBidders oldState =
    let
        newCurrent =
            List.head oldState.nextBidder
    in
    case newCurrent of
        Nothing ->
            oldState

        Just new ->
            { oldState
                | currBidder = new
                , nextBidder =
                    List.drop 1 oldState.nextBidder
                        ++ [ oldState.currBidder ]
                        |> declineSort
            }


nextAmount : Float -> Float -> Direction -> Float
nextAmount old step direction =
    case direction of
        Up ->
            old + step

        Down ->
            old - step


declineSort : List Bidder -> List Bidder
declineSort bidders =
    List.sortWith declinedSortHelper bidders


declinedSortHelper : Bidder -> Bidder -> Order
declinedSortHelper a b =
    if isDeclined a == isDeclined b then
        EQ

    else if not (isDeclined a) && isDeclined b then
        LT

    else
        GT


hasWinner : AuctionState -> Bool
hasWinner state =
    if isDeclined state.currBidder && allDeclined state.nextBidder then
        False

    else if not (allDeclined state.nextBidder) then
        False

    else
        True


allDeclined : List Bidder -> Bool
allDeclined bidders =
    (List.filter isDeclined bidders |> List.length) == 0


isDeclined : Bidder -> Bool
isDeclined bidder =
    case bidder.status of
        Declined _ ->
            True

        _ ->
            False


initialModel : Model
initialModel =
    Setup
        { people = []
        , totalRent = Nothing
        , roomNames = []
        }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] []
