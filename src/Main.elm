module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, p, text)



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
    }



-- UPDATE


type Msg
    = NoOp
    | SetTotalRent Float
    | AddParticipant Participant
    | DeleteParticipant Participant
    | AddRoom Room
    | DeleteRoom Room


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetTotalRent x ->
            ( { model | totalRent = Just x }, Cmd.none )

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
    h1 [] [ text "this is it" ]
