module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Random


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \x -> Sub.none
        }


type alias Model =
    { ecosystem : List (List Bool)
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        [ [ False, False, False, False ]
        , [ False, False, False, False ]
        , [ False, False, False, False ]
        , [ False, False, False, False ]
        ]
    , Cmd.none
    )


type Msg
    = RandomizeBoard
    | ChangeCellTo Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomizeBoard ->
            ( model
            , Random.generate ChangeCellTo Random.bool
            )

        ChangeCellTo on ->
            ( { model | ecosystem = ecoWithFirstSpot model.ecosystem on }
            , Cmd.none
            )


ecoWithFirstSpot : List (List Bool) -> Bool -> List (List Bool)
ecoWithFirstSpot ecosystem on =
    case ecosystem of
        [] ->
            []

        firstRow :: restOfTheRows ->
            case firstRow of
                [] ->
                    [] :: restOfTheRows

                firstCell :: restOfCells ->
                    (on :: restOfCells) :: restOfTheRows


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (toString model.ecosystem) ]
        , button [ onClick RandomizeBoard ] [ text "Randomize first spot on board" ]
        ]
