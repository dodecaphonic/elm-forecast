module Forecast where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp
import Signal exposing (Address)

type Action = NoOp
            | SelectLocation Location

type alias Location = { name : String
                      , latitude : Float
                      , longitude : Float
                      , id : Int
                      }


type alias Model = { locations : List Location
                   , selectedLocation : Maybe Location
                   , nextID : Int
                   }


initialModel : Model
initialModel = { locations = [
                  { name = "Rio de Janeiro"
                  , latitude = -22.9068
                  , longitude = -43.1729
                  , id = 1
                  }
                 ]
               , selectedLocation = Nothing
               , nextID = 2
               }


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    SelectLocation id ->
      { model | selectedLocation <- Just id }


-- VIEW --


locationItem : Address Action -> Maybe Location -> Location -> Html
locationItem address selectedLocation location =
  li []
    [ span
        [ onClick address (SelectLocation location) ]
        [ text location.name  ]
    ]


weatherView : Model -> Html
weatherView model =
  case model.selectedLocation of
    Nothing ->
      noLocationSelected

    Just location ->
      selectedLocation location


noLocationSelected : Html
noLocationSelected =
  div [ ] [ text "Please select a location." ]


selectedLocation : Location -> Html
selectedLocation location =
  div [ ] [ text location.name ]


locationList : Address Action -> Model -> Html
locationList address model =
  let
    maybeSelected = locationItem address model.selectedLocation
  in
    ul
      []
      (List.map maybeSelected model.locations)


view : Address Action -> Model -> Html
view address model =
  div
    [ class "container" ]
    [
      locationList address model
    , weatherView model
    ]


main = StartApp.start { view = view
                      , update = update
                      , model = initialModel
                      }
