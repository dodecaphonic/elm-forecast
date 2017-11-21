module Forecast.Widgets exposing (forecast)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css)
import Forecast.DarkSky as DS
import Forecast.Location exposing (Location)
import Forecast.Messages exposing (Msg)


forecast : Location -> DS.CompleteForecast -> Html Msg
forecast location forecast =
    div
        []
        [ currently location forecast.currently
        , hourly forecast.hourly
        ]


forecastDivStyle : List Style
forecastDivStyle =
    [ displayFlex
    , flexDirection column
    , alignItems center
    , justifyContent center
    , borderRadius2 (px 5) (px 5)
    ]


currently : Location -> DS.Forecast -> Html Msg
currently location forecast =
    div
        [ css
            ((temperature forecast.temperature)
                ++ forecastDivStyle
            )
        ]
        [ summary location forecast
        , details forecast
        ]


hourly : DS.TimespanForecast DS.HourlyForecast -> Html Msg
hourly ts =
    div [] []


summary : Location -> DS.Forecast -> Html Msg
summary location forecast =
    div
        [ css
            [ width (pct 100)
            , displayFlex
            , alignItems center
            ]
        ]
        [ i
            [ class ("wi " ++ (summaryIcon forecast.icon))
            , css
                [ fontSize (Css.em 2.4)
                , marginLeft (px 15)
                , marginTop (px 20)
                ]
            ]
            []
        , span
            [ css
                [ flex (int 2) ]
            ]
            [ text location.name ]
        , span
            [ css
                [ textAlign right
                , fontSize (Css.em 2.4)
                ]
            ]
            [ text ((toString <| truncate forecast.temperature) ++ "º")
            ]
        ]


details : DS.Forecast -> Html Msg
details forecast =
    div
        [ css
            [ displayFlex
            , margin2 (px 30) (px 30)
            , width (pct 100)
            ]
        ]
        [ wind forecast.windSpeed forecast.windBearing
        , precipitation forecast.precipIntensity forecast.precipProbability
        , pressure forecast.pressure
        ]


wind : Float -> Float -> Html Msg
wind speed bearing =
    let
        icon =
            windBearing bearing

        title =
            (toString speed) ++ " km/h"

        subtitle =
            bearingDescription bearing
    in
        dataPoint icon title subtitle


bearingDescription : Float -> String
bearingDescription bearing =
    if bearing == 0 || bearing == 360 then
        "North"
    else if bearing > 0 && bearing < 90 then
        "North East"
    else if bearing == 90 then
        "North East"
    else if bearing > 90 && bearing < 180 then
        "South East"
    else if bearing == 180 then
        "South"
    else if bearing > 180 && bearing < 270 then
        "South West"
    else if bearing == 270 then
        "West"
    else
        "North West"


windBearing : Float -> String
windBearing bearing =
    let
        icon =
            if bearing == 0 || bearing == 360 then
                "wi-wind-default _0-deg"
            else if bearing > 0 && bearing < 90 then
                "wi-wind-default _15-deg"
            else if bearing == 90 then
                "wi-wind-default _90-deg"
            else if bearing > 90 && bearing < 180 then
                "wi-wind-default _120-deg"
            else if bearing == 180 then
                "wi-wind-default _180-deg"
            else if bearing > 180 && bearing < 270 then
                "wi-wind-default _210-deg"
            else if bearing == 270 then
                "wi-wind-default _270-deg"
            else
                "wi-wind-default _345-deg"
    in
        "wi " ++ icon


precipitation : Float -> Float -> Html Msg
precipitation intensity probability =
    let
        icon =
            "wi wi-umbrella"

        title =
            (toString probability) ++ "%"

        subtitle =
            (toString intensity) ++ " cm"
    in
        dataPoint icon title subtitle


pressure : Float -> Html Msg
pressure press =
    let
        icon =
            "wi wi-down"

        title =
            (toString press) ++ " mb"

        subtitle =
            "Pressure"
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
        [ css
            [ displayFlex
            , flex2 (int 1) (int 1)
            , fontSize (Css.em 0.9)
            ]
        ]
        [ div
            [ css
                [ fontSize (Css.em 1.5) ]
            ]
            [ i [ class iconClass ] [] ]
        , div
            [ css
                [ marginLeft (px 10) ]
            ]
            [ span
                [ css
                    [ display block ]
                ]
                [ text title ]
            , span
                [ css
                    [ fontWeight (int 300) ]
                ]
                [ text subtitle ]
            ]
        ]


temperature : Float -> List Style
temperature temp =
    if temp <= 0 then
        [ backgroundColor (hex "#FAFAFA")
        , color (hex "#000000")
        ]
    else if temp > 0 && temp <= 15 then
        [ backgroundColor (hex "#AFDCD8")
        , color (hex "#000000")
        ]
    else if temp > 15 && temp < 30 then
        [ backgroundColor (hex "#ECC055") ]
    else
        [ backgroundColor (hex "#E9B96F") ]
