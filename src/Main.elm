module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, input, p, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)



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
    , currentOffer : Maybe Participant
    , formRent : String
    , formParticipant : String
    }


type alias Participant =
    { name : String
    , price : Maybe Float
    , assignment : Maybe Room
    }


type alias Room =
    { title : String }


initialModel : Model
initialModel =
    { totalRent = Nothing
    , participants = []
    , rooms = []
    , currentOffer = Nothing
    , formRent = ""
    , formParticipant = ""
    }



-- UPDATE


type Msg
    = NoOp
    | SetFormRent String
    | SetFormParticipant String
    | SetTotalRent String
    | AddParticipant String
    | DeleteParticipant Participant
    | AddRoom Room
    | DeleteRoom Room


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetFormRent rentString ->
            ( { model | formRent = rentString }, Cmd.none )

        SetFormParticipant string ->
            ( { model | formParticipant = string }, Cmd.none )

        SetTotalRent rentString ->
            ( { model | totalRent = String.toFloat rentString }, Cmd.none )

        AddParticipant new ->
            ( { model | participants = List.append model.participants [ new ] }, Cmd.none )

        DeleteParticipant person ->
            ( model, Cmd.none )

        AddRoom newRoom ->
            ( { model | rooms = List.append model.rooms [ newRoom ] }, Cmd.none )

        DeleteRoom room ->
            ( model, Cmd.none )



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
        ]


participantCreator : Model -> Html Msg
participantCreator model =
    div []
        [ input [ placeholder "Enter Participant", value model.formParticipant, onInput SetFormParticipant ] []
        , button [] [ text "Add" ]
        ]


rentSetter : Model -> Html Msg
rentSetter model =
    div []
        [ input [ placeholder "Total Rent", value model.formRent, onInput SetFormRent ] []
        , button [ onClick (SetTotalRent model.formRent) ] [ text "Set" ]
        ]
