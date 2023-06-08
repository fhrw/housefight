module Main exposing (main)

import Auction exposing (HistoryItem(..), cycleActive, loggifyHistoryItem, parseHistoryLogItem)
import Browser
import Html exposing (Html, button, div, h1, h2, h3, input, p, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Participant exposing (Participant)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


type UiState
    = ShowInput
    | Auction


type alias Model =
    { totalRent : Maybe Float
    , participants : List Participant
    , rooms : List Room
    , unAllocatedRent : Float
    , bidHistory : List HistoryItem
    , activeBidders : List Participant
    , factor : Float

    -- FORM INPUTS
    , formRent : String
    , formParticipant : String
    , formRoom : String

    -- UI
    , uiState : UiState
    }


type alias Room =
    { title : String }


initialModel : Model
initialModel =
    { totalRent = Nothing
    , participants = []
    , rooms = []
    , unAllocatedRent = 0.0
    , bidHistory = [ Bid 40.0 { name = "Steve" }, Fold { name = "Gerry" }, Bid 35.0 { name = "Eustace" } ]
    , activeBidders = []
    , factor = 1.0

    -- form
    , formRent = ""
    , formParticipant = ""
    , formRoom = ""

    -- UI
    , uiState = ShowInput
    }


validateModel : Model -> Bool
validateModel model =
    let
        roomLength =
            List.length model.rooms

        partsLength =
            List.length model.participants
    in
    roomLength > 0 && partsLength > 0 && roomLength == partsLength



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
    | ReceiveHistory HistoryItem


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
                    { title = newRoomString }
            in
            ( { model
                | rooms = List.append model.rooms [ newRoom ]
                , formRoom = ""
              }
            , Cmd.none
            )

        DeleteRoom room ->
            ( model, Cmd.none )

        ReceiveHistory item ->
            case item of
                Bid amount person ->
                    ( { model
                        | bidHistory = Bid amount person :: model.bidHistory
                        , activeBidders = cycleActive model.activeBidders
                      }
                    , Cmd.none
                    )

                Fold _ ->
                    ( { model | bidHistory = item :: model.bidHistory, activeBidders = List.drop 1 model.activeBidders }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ rentSetter model
        , participantCreator model
        , roomAdder model
        , totalPriceView model.totalRent
        , participantViewer model.participants
        , roomListViewer model.rooms
        , auctionDashboard model
        , historyLogView model.bidHistory
        , button [ onClick (ReceiveHistory (Bid 500.0 { name = "Nelly" })) ] [ text "test history add" ]
        ]


auctionDashboard : Model -> Html Msg
auctionDashboard model =
    div []
        [ h2 [] [ text "Auction" ]
        , p [] [ text "Foo, do you accept room Bar for $X?" ]
        , button [] [ text "Accept" ]
        , button [] [ text "Deny" ]
        ]


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
            ""

        Just price ->
            String.fromFloat price


totalPriceView : Maybe Float -> Html Msg
totalPriceView price =
    div []
        [ h3 []
            [ text "Total Price" ]
        , p
            []
            [ text (totalPriceToString price) ]
        ]


roomAdder : Model -> Html Msg
roomAdder model =
    div []
        [ input [ onInput SetFormRoom, placeholder "Create a new room", value model.formRoom ] []
        , button [ onClick (AddRoom model.formRoom) ] [ text "Add Room" ]
        ]


roomListViewer : List Room -> Html Msg
roomListViewer rooms =
    div []
        (List.map (\room -> p [] [ text room.title ]) rooms)


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


participantViewer : List Participant -> Html Msg
participantViewer parts =
    div []
        (List.map
            (\element -> p [] [ text element.name ])
            parts
        )
