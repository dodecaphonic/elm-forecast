module Forecast where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp
import Signal exposing (Address)
import Task
import Http

import Forecast.DarkSkySignal exposing (queryForecast, newForecast)
import Forecast.Location exposing (Location)
import Forecast.DarkSky exposing (CompleteForecast)


type Action = NoOp
            | SelectLocation Int


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


locationItem : Address Action -> Location -> Html
locationItem address location =
  li [ classList [ ("selected", location.isSelected) ] ]
     [ span
       [ onClick queryForecast.address (Just location) ]
       [ text location.name ]
     ]


weatherView : Maybe Location -> Maybe CompleteForecast -> Html
weatherView location forecast =
  case location of
    Nothing ->
      noLocationSelected

    Just loc ->
      selectedLocation loc forecast


noLocationSelected : Html
noLocationSelected =
  div [ ] [ text "Please select a location." ]


selectedLocation : Location -> Maybe CompleteForecast -> Html
selectedLocation location forecast =
  div [ ] [(completeForecast forecast)]


completeForecast : Maybe CompleteForecast -> Html
completeForecast cf =
  case cf of
    Nothing ->
      div [ ] [ text "strangely empty" ]

    Just forecast ->
      div [ ] [ text "something" ]


locationList : Address Action -> Model -> Html
locationList address model =
  ul [ ] (List.map (locationItem address) model.locations)


view : Address Action -> Model -> (Maybe Location) -> (Maybe CompleteForecast) -> Html
view address model selectedLocation forecast =
  div
  [ class "container" ]
  [
   locationList address model
  , weatherView selectedLocation forecast
  ]


main : Signal Html
main =
  let
    actions =
      Signal.mailbox Nothing

    address =
      Signal.forwardTo actions.address Just

    model =
      Signal.foldp
              (\(Just action) model -> update action model)
              initialModel
              actions.signal
  in
    Signal.map3 (view address) model queryForecast.signal newForecast.signal

