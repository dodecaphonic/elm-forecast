module Forecast exposing(..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (Cmd)
import Task
import Http

import Forecast.DarkSkyApi exposing (queryForecast)
import Forecast.Messages exposing (Msg(..))
import Forecast.Geocoding exposing (GeoLocation)
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

init : (Model, Cmd Msg)
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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    SelectLocation location ->
      let
        updateSelection loc = { loc | isSelected = loc == location }
      in
        ({ model | locations = List.map updateSelection model.locations
                , currentForecast = Nothing
         }, queryForecast location)

    UpdateForecastSucceed cf ->
      ({ model | currentForecast = Just cf }, Cmd.none)

    UpdateForecastFail _ ->
      (model, Cmd.none)

    ShowGeocodingOptions opts ->
      ({ model | currentGeocodingOptions = opts }, Cmd.none)

    GeocodeLocation loc ->
      ({ model | geocodingInput = loc }, Cmd.none)


-- VIEW --


locationItem : Location -> Html Msg
locationItem location =
  div
    [ classList [ ("selected", location.isSelected), ("location", True) ]
    , onClick (SelectLocation location) ]
    [ div
        [ class "data temp-warm" ]
        [ div [ class "place" ] [ text location.name ] ]
    ]


weatherView : Maybe Location -> Maybe DS.CompleteForecast -> Html Msg
weatherView location forecast =
  case location of
    Nothing ->
      noLocationSelected

    Just loc ->
      selectedLocation loc forecast


noLocationSelected : Html Msg
noLocationSelected =
  div [ class "forecast-container" ] [ text "Please select a location." ]


selectedLocation : Location -> Maybe DS.CompleteForecast -> Html Msg
selectedLocation location forecast =
  div [ class "forecast-container" ] [(completeForecast location forecast)]


completeForecast : Location -> Maybe DS.CompleteForecast -> Html Msg
completeForecast location cf =
  case cf of
    Nothing ->
      div [ ] [ text "Weather goes here." ]

    Just forecast ->
      W.forecast location forecast


locationList : Model -> Html Msg
locationList model =
  div
    [ class "locations" ]
    (List.map locationItem model.locations)


view : Model -> Html Msg
view model =
  let
    selectedLocation =
      model.locations
        |> List.filter .isSelected
        |> List.head
  in
    div
    [ class "container" ]
    [
      locationList model
    , weatherView selectedLocation model.currentForecast
    ]


main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none }
