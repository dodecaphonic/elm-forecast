module Forecast.DarkSky(completeForecastDecoder, CompleteForecast, TimespanForecast, Forecast, PossibleForecast) where


import Json.Decode as Json exposing ((:=))


type alias CompleteForecast = { latitude : Float
                              , longitude : Float
                              , timezone : String
                              , currently : Forecast
                              , hourly : TimespanForecast
                              , daily : TimespanForecast
                              }


type alias TimespanForecast = { summary : String
                              , icon : String
                              , forecasts : List PossibleForecast }


type alias Forecast = { time : Int
                      , summary : String
                      , icon : String
                      , precipIntensity : Float
                      , precipProbability : Float
                      , temperature : Float
                      , windSpeed : Float
                      , windBearing : Float
                      }


type alias PossibleForecast = { time : Int
                              , summary : String
                              , icon : String
                              , precipIntensity : Float
                              , precipProbability : Float
                              , temperatureMin : Float
                              , temperatureMax : Float
                              }


timespanForecastDecoder : Json.Decoder TimespanForecast
timespanForecastDecoder =
  Json.object3
    TimespanForecast
    ("summary" := Json.string)
    ("icon" := Json.string)
    ("data" := Json.list possibleForecastDecoder)


forecastDecoder : Json.Decoder Forecast
forecastDecoder =
  Json.object8
    Forecast
    ("time" := Json.int)
    ("summary" := Json.string)
    ("icon" := Json.string)
    ("precipIntensity" := Json.float)
    ("precipProbability" := Json.float)
    ("temperature" := Json.float)
    ("windSpeed" := Json.float)
    ("windBearing" := Json.float)


possibleForecastDecoder : Json.Decoder PossibleForecast
possibleForecastDecoder =
  Json.object7
    PossibleForecast
    ("time" := Json.int)
    ("summary" := Json.string)
    ("icon" := Json.string)
    ("precipIntensity" := Json.float)
    ("precipProbability" := Json.float)
    ("temperatureMin" := Json.float)
    ("temperatureMax" := Json.float)


completeForecastDecoder : Json.Decoder (Maybe CompleteForecast)
completeForecastDecoder =
  Json.maybe <|
    Json.object6
      CompleteForecast
      ("latitude" := Json.float)
      ("longitude" := Json.float)
      ("timezone" := Json.string)
      ("currently" := forecastDecoder)
      ("hourly" := timespanForecastDecoder)
      ("daily" := timespanForecastDecoder)
