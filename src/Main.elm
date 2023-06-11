module Main exposing (main)

import Auction exposing (HistoryItem(..), loggifyHistoryItem, parseHistoryLogItem)
import Browser
import Html exposing (Html, button, div, h1, h2, h3, input, p, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Participant exposing (Participant)
import Platform.Cmd as Cmd
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
    | Auction AuctionOptions
    | Results


type alias SetupFields =
    { people : List String
    , totalRent : Float
    , roomNames : List String
    }


type alias AuctionOptions =
    { max : Float
    , people : List String
    , currPrice : Float
    }


type alias Bidder =
    { person : String, status : BidStatus }


type BidStatus
    = Active
    | Declined


initialModel : Model
initialModel =
    Setup



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


historyLogView : List HistoryItem -> Html Msg
historyLogView history =
    div []
        [ h3 []
            [ text "History" ]
        , div
            []
            (List.map (\x -> p [] [ text (loggifyHistoryItem x |> parseHistoryLogItem) ]) history)
        ]
