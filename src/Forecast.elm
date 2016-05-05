module Forecast where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp
import Effects exposing (Effects)
import Signal exposing (Address)
import Task
import Http

import Forecast.DarkSkyApi exposing (queryForecast)
import Forecast.Actions exposing (Action(..))
import Forecast.Geocoding exposing (queryGeocoding, newGeocoding, GeoLocation)
import Forecast.Location exposing (Location)
import Forecast.Locations exposing (rio, london)
import Forecast.DarkSky as DS
import Forecast.Widgets as W


type alias Model = { locations : List Location
                   , nextID : Int
                   , currentForecast : (Maybe DS.CompleteForecast)
                   , currentGeocodingOptions : (List GeoLocation)
                   , geocodingInput : String
                   }

init : (Model, Effects Action)
init =
  let
    locations = [{ rio | id = 1, isSelected = True },
                 { london | id = 2 }]
    startingLocation = locations |> List.head |> Maybe.withDefault rio
  in
    (initialModel locations, queryForecast startingLocation)


initialModel : List Location -> Model
initialModel locations = { locations = locations
                         , nextID = (List.length locations) + 1
                         , currentForecast = Nothing
                         , currentGeocodingOptions = []
                         , geocodingInput = ""
                         }


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)

    SelectLocation location ->
      let
        updateSelection loc = { loc | isSelected = loc == location }
      in
        ({ model | locations = List.map updateSelection model.locations
                , currentForecast = Nothing
         }, queryForecast location)

    UpdateForecast cf ->
      ({ model | currentForecast = cf }, Effects.none)

    ShowGeocodingOptions opts ->
      ({ model | currentGeocodingOptions = opts }, Effects.none)

    GeocodeLocation loc ->
      ({ model | geocodingInput = loc }, Effects.none)


-- VIEW --


locationItem : Address Action -> Location -> Html
locationItem address location =
  div
    [ classList [ ("selected", location.isSelected), ("location", True) ]
    , onClick address (SelectLocation location) ]
    [ div
        [ class "data temp-warm" ]
        [ div [ class "place" ] [ text location.name ] ]
    ]


weatherView : Maybe Location -> Maybe DS.CompleteForecast -> Html
weatherView location forecast =
  case location of
    Nothing ->
      noLocationSelected

    Just loc ->
      selectedLocation loc forecast


noLocationSelected : Html
noLocationSelected =
  div [ class "forecast-container" ] [ text "Please select a location." ]


selectedLocation : Location -> Maybe DS.CompleteForecast -> Html
selectedLocation location forecast =
  div [ class "forecast-container" ] [(completeForecast location forecast)]


completeForecast : Location -> Maybe DS.CompleteForecast -> Html
completeForecast location cf =
  case cf of
    Nothing ->
      div [ ] [ text "Weather goes here." ]

    Just forecast ->
      W.forecast location forecast


locationList : Address Action -> Model -> Html
locationList address model =
  div [ class "locations" ] (List.map (locationItem address) model.locations)


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
    , weatherView selectedLocation model.currentForecast
    ]


actions : Signal.Mailbox (Maybe Action)
actions =
  Signal.mailbox Nothing


app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [] }


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


main : Signal Html
main = app.html
