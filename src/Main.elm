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


type alias Model =
    { totalRent : Maybe Float
    , participants : List Participant
    , rooms : List Room
    , bidHistory : List HistoryItem

    -- AUCTION
    , remainderRent : Float
    , factor : Float

    -- FORM INPUTS
    , formRent : String
    , formParticipant : String
    , formRoom : String

    -- UI
    , uiState : UiState
    }


initialModel : Model
initialModel =
    { totalRent = Just 400
    , participants = [ { name = "Steve" }, { name = "Emily" } ]
    , rooms =
        [ { title = "front room", status = Allocated { name = "Steve" } }
        , { title = "back room", status = Unallocated }
        ]
    , bidHistory = []

    -- AUCTION
    , remainderRent = 0 --unset
    , factor = 1.0

    -- FORM
    , formRent = ""
    , formParticipant = ""
    , formRoom = ""

    -- UI
    , uiState = ShowInput
    }


type UiState
    = ShowInput
    | Auction


type alias Offer =
    { person : Participant
    , amount : Float
    , room : Room
    }


validateModel : Model -> Bool
validateModel model =
    let
        roomLength =
            List.length model.rooms

        partsLength =
            List.length model.participants

        setPrice =
            model.totalRent /= Nothing
    in
    roomLength > 0 && partsLength > 0 && roomLength == partsLength && setPrice



-- UPDATE


type Msg
    = NoOp
    | SetFormRent String
    | SetFormParticipant String
    | SetFormRoom String
    | SetTotalRent String
    | AddParticipant String
    | DeleteParticipant Participant
    | AddRoom String
    | DeleteRoom Room
    | StartAuction
    | ReceiveHistory HistoryItem


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetFormRent rentString ->
            ( { model | formRent = rentString }, Cmd.none )

        SetFormParticipant string ->
            ( { model | formParticipant = string }, Cmd.none )

        SetFormRoom string ->
            ( { model | formRoom = string }, Cmd.none )

        SetTotalRent rentString ->
            ( { model
                | totalRent = String.toFloat rentString
                , formRent = ""
              }
            , Cmd.none
            )

        AddParticipant nameString ->
            let
                new : Participant
                new =
                    { name = nameString }
            in
            ( { model
                | participants = List.append model.participants [ new ]
                , formParticipant = ""
              }
            , Cmd.none
            )

        DeleteParticipant person ->
            ( model, Cmd.none )

        AddRoom newRoomString ->
            let
                newRoom : Room
                newRoom =
                    { title = newRoomString, status = Unallocated }
            in
            ( { model
                | rooms = List.append model.rooms [ newRoom ]
                , formRoom = ""
              }
            , Cmd.none
            )

        DeleteRoom room ->
            ( model, Cmd.none )

        StartAuction ->
            case model.totalRent of
                Nothing ->
                    ( model, Cmd.none )

                Just rent ->
                    ( { model
                        | uiState = Auction
                      }
                    , Cmd.none
                    )

        ReceiveHistory item ->
            ( { model | bidHistory = item :: model.bidHistory }, Cmd.none )


findToAuction : List Room -> Maybe Room
findToAuction rooms =
    List.filter (\x -> x.status == Unallocated) rooms
        |> List.head



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.uiState of
        ShowInput ->
            div []
                [ rentSetter model
                , participantCreator model
                , roomAdder model
                , button [ onClick StartAuction ] [ text "Start" ]
                ]

        Auction ->
            div []
                [ showCurrentOffer { person = { name = "Steve" }, amount = 450.0, room = { title = "front room", status = Unallocated } }
                , button [ onClick (ReceiveHistory (Bid { amount = 450, bidder = { name = "Steve" } })) ] [ text "accept" ]
                , button [ onClick (ReceiveHistory (Fold { name = "Steve" })) ] [ text "decline" ]
                , historyLogView model.bidHistory
                ]


showCurrentOffer : Offer -> Html Msg
showCurrentOffer offer =
    div []
        [ p [] [ text (offer.person.name ++ " would you like to bid " ++ String.fromFloat offer.amount ++ " for " ++ roomToString offer.room) ]
        ]


roomToString : Room -> String
roomToString room =
    room.title


historyLogView : List HistoryItem -> Html Msg
historyLogView history =
    div []
        [ h3 []
            [ text "History" ]
        , div
            []
            (List.map (\x -> p [] [ text (loggifyHistoryItem x |> parseHistoryLogItem) ]) history)
        ]


totalPriceToString : Maybe Float -> String
totalPriceToString mFloat =
    case mFloat of
        Nothing ->
            "Price not set"

        Just price ->
            String.fromFloat price


roomAdder : Model -> Html Msg
roomAdder model =
    div []
        [ input [ onInput SetFormRoom, placeholder "Create a new room", value model.formRoom ] []
        , button [ onClick (AddRoom model.formRoom) ] [ text "Add Room" ]
        ]


rentSetter : Model -> Html Msg
rentSetter model =
    div []
        [ input [ placeholder "Total Rent", value model.formRent, onInput SetFormRent ] []
        , button [ onClick (SetTotalRent model.formRent) ] [ text "Set" ]
        ]


participantCreator : Model -> Html Msg
participantCreator model =
    div []
        [ input [ placeholder "Enter Participant", value model.formParticipant, onInput SetFormParticipant ] []
        , button [ onClick (AddParticipant model.formParticipant) ] [ text "Add" ]
        ]
