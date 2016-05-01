module Forecast where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp
import Signal exposing (Address)
import Task
import Http

import Forecast.DarkSkySignal exposing (queryForecast, newForecast)
import Forecast.Geocoding exposing (queryGeocoding, newGeocoding, GeoLocation)
import Forecast.Location exposing (Location)
import Forecast.DarkSky as DS
import Forecast.Widgets as W


type Action = NoOp
            | SelectLocation Int
            | UpdateForecast (Maybe DS.CompleteForecast)
            | GeocodeLocation String
            | ShowGeocodingOptions (List GeoLocation)


type alias Model = { locations : List Location
                   , nextID : Int
                   , currentForecast : (Maybe DS.CompleteForecast)
                   , currentGeocodingOptions : (List GeoLocation)
                   , geocodingInput : String
                   }


initialModel : Model
initialModel = { locations = [
                  { name = "Rio de Janeiro"
                  , latitude = -22.9068
                  , longitude = -43.1729
                  , id = 1
                  , isSelected = False
                  }
                  ,
                  { name = "London"
                  , latitude = 51.5072
                  , longitude = -0.1275
                  , id = 2
                  , isSelected = False
                  }
                 ]
               , nextID = 3
               , currentForecast = Nothing
               , currentGeocodingOptions = []
               , geocodingInput = ""
               }


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    SelectLocation id ->
      let
        updateSelection loc = { loc | isSelected = loc.id == id }
      in
        { model | locations = List.map updateSelection model.locations
                , currentForecast = Nothing
        }

    UpdateForecast cf ->
      { model | currentForecast = cf }

    ShowGeocodingOptions opts ->
      { model | currentGeocodingOptions = opts }

    GeocodeLocation loc ->
      { model | geocodingInput = loc }


-- VIEW --


locationItem : Address (Maybe Location) -> Location -> Html
locationItem address location =
  div
    [ classList [ ("selected", location.isSelected), ("location", True) ]
    , onClick address (Just location) ]
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


locationList : Address (Maybe Location) -> Model -> Html
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
      locationList queryForecast.address model
    , weatherView selectedLocation model.currentForecast
    ]


actions : Signal.Mailbox (Maybe Action)
actions =
  Signal.mailbox Nothing


updates =
  let
    locationToAction loc =
      (Maybe.map (\l -> Just (SelectLocation l.id)) loc)
        |> Maybe.withDefault (Just NoOp)

    geocodingToAction geo =
      (Maybe.map (\g -> Just (ShowGeocodingOptions g)) geo)
        |> Maybe.withDefault (Just (ShowGeocodingOptions []))

    nfs = Signal.map (\cf -> Just (UpdateForecast cf)) newForecast.signal
    qfs = Signal.map locationToAction queryForecast.signal
    qgcs = Signal.map (\add -> Just (GeocodeLocation add)) queryGeocoding.signal
    ngcs = Signal.map (\add -> Just (GeocodeLocation add)) queryGeocoding.signal
  in
    Signal.mergeMany [actions.signal, qfs, nfs, qgcs, ngcs]


main : Signal Html
main =
  let
    address =
      Signal.forwardTo actions.address Just

    modelShouldChange ma model =
      Maybe.map (\action -> update action model) ma |> Maybe.withDefault model

    model =
      Signal.foldp
        modelShouldChange
        initialModel
        updates
  in
    Signal.map (view address) model
