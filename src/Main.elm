module Main exposing (BidStatus(..), declineSort, declinedSortHelper, main)

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


type alias AuctionState =
    { max : Float
    , currBidder : Bidder
    , nextBidder : List Bidder
    , currPrice : Float
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


newAmount : Float -> Float -> Direction -> Float
newAmount old step direction =
    let
        offset =
            step * 0.5
    in
    case direction of
        Up ->
            old + offset

        Down ->
            old - offset


hasWinner : AuctionState -> Bool
hasWinner state =
    if isDeclined state.currBidder && allDeclined state.nextBidder then
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
