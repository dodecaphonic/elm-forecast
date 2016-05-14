module Forecast.Widgets exposing (forecast)


import Html exposing (div, span, i, text, Html)
import Html.Attributes exposing (class)

import Forecast.DarkSky as DS
import Forecast.Location exposing (Location)
import Forecast.Messages exposing (Msg)


forecast : Location -> DS.CompleteForecast -> Html Msg
forecast location forecast =
  div
    [ ]
    [ currently location forecast.currently
    , hourly forecast.hourly
    ]


currently : Location -> DS.Forecast -> Html Msg
currently location forecast =
  div
    [ class ("forecast " ++ (temperature forecast.temperature)) ]
    [ summary location forecast
    , details forecast
    ]


hourly : DS.TimespanForecast DS.HourlyForecast -> Html Msg
hourly ts = div [ ] [ ]


summary : Location -> DS.Forecast -> Html Msg
summary location forecast =
  div
    [ class "summary" ]
    [ i [ class ("conditions wi " ++ (summaryIcon forecast.icon)) ] [ ]
    , span [ class "location" ] [ text location.name ]
    , span [ class "temperature" ] [ text ((toString <| truncate forecast.temperature) ++ "º") ]
    ]


details : DS.Forecast -> Html Msg
details forecast =
  div
    [ class "details" ]
    [ wind forecast.windSpeed forecast.windBearing
    , precipitation forecast.precipIntensity forecast.precipProbability
    , pressure forecast.pressure
    ]


wind : Float -> Float -> Html Msg
wind speed bearing =
  let
    icon = windBearing bearing
    title = (toString speed) ++ " km/h"
    subtitle = bearingDescription bearing
  in
    dataPoint icon title subtitle


bearingDescription : Float -> String
bearingDescription bearing =
  if bearing == 0 || bearing == 360 then "North"
  else if bearing > 0 && bearing < 90 then "North East"
  else if bearing == 90 then "North East"
  else if bearing > 90 && bearing < 180 then "South East"
  else if bearing == 180 then "South"
  else if bearing > 180 && bearing < 270 then "South West"
  else if bearing == 270 then "West"
  else "North West"


windBearing : Float -> String
windBearing bearing =
  let
    icon =
      if bearing == 0 || bearing == 360 then "wi-wind-default _0-deg"
      else if bearing > 0 && bearing < 90 then "wi-wind-default _15-deg"
      else if bearing == 90 then "wi-wind-default _90-deg"
      else if bearing > 90 && bearing < 180 then "wi-wind-default _120-deg"
      else if bearing == 180 then "wi-wind-default _180-deg"
      else if bearing > 180 && bearing < 270 then "wi-wind-default _210-deg"
      else if bearing == 270 then "wi-wind-default _270-deg"
      else "wi-wind-default _345-deg"
  in
    "wi " ++ icon


precipitation : Float -> Float -> Html Msg
precipitation intensity probability =
  let
    icon = "wi wi-umbrella"
    title = (toString probability) ++ "%"
    subtitle = (toString intensity) ++ " cm"
  in
    dataPoint icon title subtitle


pressure : Float -> Html Msg
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


dataPoint : String -> String -> String -> Html Msg
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
  if temp <= 0 then "temp-cold"
  else if temp > 0 && temp <= 15 then "temp-cool"
  else if temp > 15 && temp < 30 then "temp-warm"
  else "temp-hot"
