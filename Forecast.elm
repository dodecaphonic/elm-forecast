module Forecast where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp
import Signal exposing (Address)

type Action = NoOp
            | SelectLocation Int


type alias Location = { name : String
                      , latitude : Float
                      , longitude : Float
                      , isSelected : Bool
                      , id : Int
                      }


type alias Model = { locations : List Location
                   , nextID : Int
                   }


initialModel : Model
initialModel = { locations = [
                  { name = "Rio de Janeiro"
                  , latitude = -22.9068
                  , longitude = -43.1729
                  , id = 1
                  , isSelected = False
                  }
                 ]
               , nextID = 2
               }


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    SelectLocation id ->
      let
        updateSelection loc = { loc | isSelected <- loc.id == id }
      in
        { model | locations <- List.map updateSelection model.locations }


-- VIEW --


locationItem : Address Action ->  Location -> Html
locationItem address location =
  li [ classList [ ("selected", location.isSelected) ] ]
     [ span
       [ onClick address (SelectLocation location.id) ]
       [ text location.name ]
     ]


weatherView : Maybe Location -> Html
weatherView location =
  case location of
    Nothing ->
      noLocationSelected

    Just loc ->
      selectedLocation loc


noLocationSelected : Html
noLocationSelected =
  div [ ] [ text "Please select a location." ]


selectedLocation : Location -> Html
selectedLocation location =
  div [ ] [ text location.name ]


locationList : Address Action -> Model -> Html
locationList address model =
  ul [ ] (List.map (locationItem address) model.locations)


view : Address Action -> Model -> Html
view address model =
  let
    selectedLocation =
      model.locations
        |> List.filter .isSelected
        |> List.head
  in
    div
      [ class "container" ]
      [
        locationList address model
      , weatherView selectedLocation
      ]


main = StartApp.start { view = view
                      , update = update
                      , model = initialModel
                      }
