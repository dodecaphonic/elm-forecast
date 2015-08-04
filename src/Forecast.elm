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
import Forecast.DarkSky as DS
import Forecast.Widgets as W


type Action = NoOp
            | SelectLocation Int
            | UpdateForecast (Maybe DS.CompleteForecast)


type alias Model = { locations : List Location
                   , nextID : Int
                   , currentForecast : (Maybe DS.CompleteForecast)
                   }


initialModel : Model
initialModel = { locations = [
                  { name = "Rio de Janeiro"
                  , latitude = -22.9068
                  , longitude = -43.1729
                  , id = 1
                  , isSelected = True
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
        { model | locations <- List.map updateSelection model.locations
                , currentForecast <- Nothing
        }

    UpdateForecast cf ->
      { model | currentForecast <- cf }


-- VIEW --


locationItem : Address (Maybe Location) -> Location -> Html
locationItem address location =
  div
    [ classList [ ("selected", location.isSelected), ("location", True) ]
    , onClick address (Just location) ]
    [ div
        [ class "data" ]
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
      div [ ] [ text "strangely empty" ]

    Just forecast ->
      div
        [ ]
        [ currently location forecast.currently
        , hourly forecast.hourly
        ]


currently : Location -> DS.Forecast -> Html
currently location forecast =
  div
    [ class "forecast" ]
    [ W.summary location forecast
    , W.details forecast
    ]


hourly : DS.TimespanForecast DS.HourlyForecast -> Html
hourly ts = div [ ] [ ]


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

    forecastToAction = Signal.map (\cf -> Just (UpdateForecast cf)) newForecast.signal
    queryToAction = Signal.map locationToAction queryForecast.signal
  in
    Signal.mergeMany [actions.signal, queryToAction, forecastToAction]


main : Signal Html
main =
  let
    address =
      Signal.forwardTo actions.address Just

    model =
      Signal.foldp
        (\(Just action) model -> update action model)
        initialModel
        updates
  in
    Signal.map (view address) model
