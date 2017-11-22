module Forecast.Widgets exposing (forecast, locationListItem)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Forecast.DarkSky as DS
import Forecast.Location exposing (Location)
import Forecast.Messages exposing (Msg(SelectLocation))


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
            ((temperatureStyles forecast.temperature)
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
            , marginTop (px 10)
            ]
        ]
        [ weatherIcon [] forecast.icon
        , span
            [ css
                [ flex (int 2)
                , marginLeft (px 15)
                ]
            ]
            [ text location.name ]
        , span
            [ css
                [ textAlign right
                , fontSize (Css.em 2.4)
                , fontWeight (int 500)
                , marginRight (px 15)
                ]
            ]
            [ text (formatTemperature forecast.temperature)
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


temperatureStyles : Float -> List Style
temperatureStyles temp =
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


formatTemperature : Float -> String
formatTemperature temp =
    (toString <| truncate temp) ++ "º"


weatherIcon : List Style -> String -> Html msg
weatherIcon styles icon =
    i
        [ class ("wi " ++ (summaryIcon icon))
        , css <|
            [ fontSize (Css.em 2.4)
            , marginLeft (px 15)
            ]
                ++ styles
        ]
        []


locationListItem : Location -> Html Msg
locationListItem location =
    let
        defaultColors =
            [ backgroundColor (rgb 224 242 255)
            , color (rgb 0 0 0)
            ]

        currentTemperature =
            .temperature << .currently

        colors =
            location.currentForecast
                |> Maybe.map (temperatureStyles << currentTemperature)
                |> Maybe.withDefault defaultColors

        temperatureValue =
            location.currentForecast
                |> Maybe.map (formatTemperature << currentTemperature)
                |> Maybe.withDefault "--"

        temperatureIcon =
            location.currentForecast
                |> Maybe.map (weatherIcon [ flex (int 1) ] << .icon << .currently)
                |> Maybe.withDefault (weatherIcon [ flex (int 1) ] "fog")
    in
        div
            [ onClick (SelectLocation location)
            , css <|
                [ height (px 120)
                , borderRadius2 (px 5) (px 5)
                , cursor pointer
                , marginBottom (px 20)
                ]
                    ++ colors
            ]
            [ div
                [ css
                    [ height (px 20)
                    , backgroundColor
                        (if location.isSelected then
                            (rgb 116 12 232)
                         else
                            (rgb 224 242 255)
                        )
                    ]
                ]
                []
            , div
                [ css
                    [ margin2 (px 10) (px 10)
                    , displayFlex
                    , alignItems center
                    , fontSize (Css.em 1.2)
                    ]
                ]
                [ temperatureIcon
                , div
                    [ css
                        [ alignSelf flexEnd
                        , fontSize (Css.em 1.4)
                        , fontWeight (int 500)
                        ]
                    ]
                    [ text temperatureValue ]
                ]
            , div
                [ css
                    [ displayFlex
                    , margin2 (px 10) (px 10)
                    ]
                ]
                [ text location.name ]
            ]
