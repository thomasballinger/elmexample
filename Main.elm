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
    , leftToRandomize : Int
    }


rows =
    4


columns =
    4


init : ( Model, Cmd Msg )
init =
    ( { ecosystem =
            [ [ False, False, False, False ]
            , [ False, False, False, False ]
            , [ False, False, False, False ]
            , [ False, False, False, False ]
            ]
      , leftToRandomize = 0
      }
    , Cmd.none
    )


type Msg
    = RandomizeBoard
    | ChangeNextCellTo Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomizeBoard ->
            ( { model | leftToRandomize = rows * columns - 1 }
            , Random.generate ChangeNextCellTo Random.bool
            )

        ChangeNextCellTo on ->
            let
                row =
                    model.leftToRandomize // 4

                col =
                    model.leftToRandomize % 4
            in
                ( { model
                    | ecosystem = ecoWithSpot row col on model.ecosystem
                    , leftToRandomize = (model.leftToRandomize - 1)
                  }
                , if model.leftToRandomize > 0 then
                    Random.generate ChangeNextCellTo Random.bool
                  else
                    Cmd.none
                )


ecoWithSpot : Int -> Int -> Bool -> List (List Bool) -> List (List Bool)
ecoWithSpot row col on ecosystem =
    case ecosystem of
        [] ->
            []

        firstRow :: restOfRows ->
            if row > 0 then
                firstRow :: (ecoWithSpot (row - 1) col on restOfRows)
            else
                case firstRow of
                    [] ->
                        [] :: restOfRows

                    firstCell :: restOfCells ->
                        if col > 0 then
                            case ecoWithSpot row (col - 1) on (restOfCells :: restOfRows) of
                                [] ->
                                    []

                                changedRow :: _ ->
                                    (firstCell :: changedRow) :: restOfRows
                        else
                            (on :: restOfCells) :: restOfRows


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (toString model.ecosystem) ]
        , button [ onClick RandomizeBoard ] [ text "Randomize entire board" ]
        ]
