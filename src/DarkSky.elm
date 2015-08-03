module Forecast.DarkSky(completeForecastDecoder, CompleteForecast, TimespanForecast, Forecast, DailyForecast, HourlyForecast) where


import Json.Decode as Json exposing ((:=))


type alias CompleteForecast = { latitude : Float
                              , longitude : Float
                              , timezone : String
                              , currently : Forecast
                              , hourly : TimespanForecast HourlyForecast
                              , daily : TimespanForecast DailyForecast
                              }


type alias HourlyForecast = { time : Int
                            , summary : String
                            , icon : String
                            , temperature : Float
                            , precipIntensity : Float
                            , precipProbability : Float
                            }


type alias TimespanForecast ft = { summary : String
                                 , icon : String
                                 , forecasts : List ft
                                 }


type alias Forecast = { time : Int
                      , summary : String
                      , icon : String
                      , precipIntensity : Float
                      , precipProbability : Float
                      , temperature : Float
                      , windSpeed : Float
                      , windBearing : Float
                      }


type alias DailyForecast = { time : Int
                           , summary : String
                           , icon : String
                           , precipIntensity : Float
                           , precipProbability : Float
                           , temperatureMin : Float
                           , temperatureMax : Float
                           }


timespanForecastDecoder : Json.Decoder ft -> Json.Decoder (TimespanForecast ft)
timespanForecastDecoder dataDecoder =
  Json.object3
    TimespanForecast
    ("summary" := Json.string)
    ("icon" := Json.string)
    ("data" := Json.list dataDecoder)


hourlyForecastDecoder : Json.Decoder HourlyForecast
hourlyForecastDecoder =
  Json.object6
    HourlyForecast
    ("time" := Json.int)
    ("summary" := Json.string)
    ("icon" := Json.string)
    ("temperature" := Json.float)
    ("precipIntensity" := Json.float)
    ("precipProbability" := Json.float)


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


dailyForecastDecoder : Json.Decoder DailyForecast
dailyForecastDecoder =
  Json.object7
    DailyForecast
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
      ("hourly" := (timespanForecastDecoder hourlyForecastDecoder))
      ("daily" := (timespanForecastDecoder dailyForecastDecoder))
