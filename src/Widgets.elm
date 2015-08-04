module Forecast.Widgets(forecast) where


import Html exposing (div, span, i, text, Html)
import Html.Attributes exposing (class)

import Forecast.DarkSky as DS
import Forecast.Location exposing (Location)


forecast : Location -> DS.CompleteForecast -> Html
forecast location forecast =
  div
    [ ]
    [ currently location forecast.currently
    , hourly forecast.hourly
    ]


currently : Location -> DS.Forecast -> Html
currently location forecast =
  div
    [ class ("forecast " ++ (temperature forecast.temperature)) ]
    [ summary location forecast
    , details forecast
    ]


hourly : DS.TimespanForecast DS.HourlyForecast -> Html
hourly ts = div [ ] [ ]


summary : Location -> DS.Forecast -> Html
summary location forecast =
  div
    [ class "summary" ]
    [ i [ class ("conditions wi " ++ (summaryIcon forecast.icon)) ] [ ]
    , span [ class "location" ] [ text location.name ]
    , span [ class "temperature" ] [ text ((toString <| truncate forecast.temperature) ++ "º") ]
    ]


details : DS.Forecast -> Html
details forecast =
  div
    [ class "details" ]
    [ wind forecast.windSpeed forecast.windBearing
    , precipitation forecast.precipIntensity forecast.precipProbability
    , pressure forecast.pressure
    ]


wind : Float -> Float -> Html
wind speed bearing =
  let
    icon = windBearing bearing
    title = (toString speed) ++ " km/h"
    subtitle = bearingDescription bearing
  in
    dataPoint icon title subtitle


bearingDescription : Float -> String
bearingDescription bearing =
  if
    | bearing == 0 || bearing == 360 -> "North"
    | bearing > 0 && bearing < 90 -> "North East"
    | bearing == 90 -> "North East"
    | bearing > 90 && bearing < 180 -> "South East"
    | bearing == 180 -> "South"
    | bearing > 180 && bearing < 270 -> "South West"
    | bearing == 270 -> "West"
    | bearing > 270 && bearing < 360 -> "North West"


windBearing : Float -> String
windBearing bearing =
  let
    icon =
      if
        | bearing == 0 || bearing == 360 -> "wi-wind-default _0-deg"
        | bearing > 0 && bearing < 90 -> "wi-wind-default _15-deg"
        | bearing == 90 -> "wi-wind-default _90-deg"
        | bearing > 90 && bearing < 180 -> "wi-wind-default _120-deg"
        | bearing == 180 -> "wi-wind-default _180-deg"
        | bearing > 180 && bearing < 270 -> "wi-wind-default _210-deg"
        | bearing == 270 -> "wi-wind-default _270-deg"
        | bearing > 270 && bearing < 360 -> "wi-wind-default _345-deg"
  in
    "wi " ++ icon


precipitation : Float -> Float -> Html
precipitation intensity probability =
  let
    icon = "wi wi-umbrella"
    title = (toString probability) ++ "%"
    subtitle = (toString intensity) ++ " cm"
  in
    dataPoint icon title subtitle


pressure : Float -> Html
pressure press =
  let
    icon = "wi wi-down"
    title = (toString press) ++ " mb"
    subtitle = "Pressure"
  in
    dataPoint icon title subtitle


summaryIcon : String -> String
summaryIcon iconName =
  case iconName of
    "clear-day" ->
      "wi-day-sunny"

    "clear-night" ->
      "wi-night-clear"

    "rain" ->
      "wi-rain"

    "snow" ->
      "wi-snow"

    "sleet" ->
      "wi-sleet"

    "wind" ->
      "wi-day-windy"

    "fog" ->
      "wi-day-fog"

    "cloudy" ->
      "wi-cloudy"

    "partly-cloudy-day" ->
      "wi-cloudy"

    "partly-cloudy-night" ->
      "wi-cloudy"

    otherwise ->
      "wi-sunset"


dataPoint : String -> String -> String -> Html
dataPoint iconClass title subtitle =
  div
    [ class "detail" ]
    [ div [ class "icon" ]
          [ i [ class iconClass ] [ ] ]
    , div [ class "text" ]
          [ span [ class "title" ] [ text title ]
          , span [ class "subtitle" ] [ text subtitle ]
          ]
    ]


temperature : Float -> String
temperature temp =
  if | temp <= 0 -> "temp-cold"
     | temp > 0 && temp <= 15 -> "temp-cool"
     | temp > 15 && temp < 30 -> "temp-warm"
     | otherwise -> "temp-hot"
