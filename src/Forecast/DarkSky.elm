module Forecast.DarkSky
    exposing
        ( completeForecastDecoder
        , CompleteForecast
        , TimespanForecast
        , Forecast
        , DailyForecast
        , HourlyForecast
        )

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)


type alias CompleteForecast =
    { latitude : Float
    , longitude : Float
    , timezone : String
    , currently : Forecast
    , hourly : TimespanForecast HourlyForecast
    , daily : TimespanForecast DailyForecast
    }


type alias HourlyForecast =
    { time : Int
    , summary : String
    , icon : String
    , temperature : Float
    , precipIntensity : Float
    , precipProbability : Float
    }


type alias TimespanForecast ft =
    { summary : String
    , icon : String
    , forecasts : List ft
    }


type alias Forecast =
    { time : Int
    , summary : String
    , icon : String
    , precipIntensity : Float
    , precipProbability : Float
    , temperature : Float
    , windSpeed : Float
    , windBearing : Float
    , humidity : Float
    , visibility : Float
    , cloudCover : Float
    , pressure : Float
    , ozone : Float
    }


type alias DailyForecast =
    { time : Int
    , summary : String
    , icon : String
    , precipIntensity : Float
    , precipProbability : Float
    , temperatureMin : Float
    , temperatureMax : Float
    }


timespanForecastDecoder : Decode.Decoder ft -> Decode.Decoder (TimespanForecast ft)
timespanForecastDecoder dataDecoder =
    Decode.succeed TimespanForecast
        |> required "summary" Decode.string
        |> required "icon" Decode.string
        |> required "data" (Decode.list dataDecoder)


hourlyForecastDecoder : Decode.Decoder HourlyForecast
hourlyForecastDecoder =
    Decode.succeed HourlyForecast
        |> required "time" Decode.int
        |> required "summary" Decode.string
        |> required "icon" Decode.string
        |> required "temperature" Decode.float
        |> required "precipIntensity" Decode.float
        |> required "precipProbability" Decode.float


forecastDecoder : Decode.Decoder Forecast
forecastDecoder =
    Decode.succeed Forecast
        |> required "time" Decode.int
        |> required "summary" Decode.string
        |> required "icon" Decode.string
        |> required "precipIntensity" Decode.float
        |> required "precipProbability" Decode.float
        |> required "temperature" Decode.float
        |> required "windSpeed" Decode.float
        |> required "windBearing" Decode.float
        |> required "humidity" Decode.float
        |> required "visibility" Decode.float
        |> required "cloudCover" Decode.float
        |> required "pressure" Decode.float
        |> required "ozone" Decode.float


dailyForecastDecoder : Decode.Decoder DailyForecast
dailyForecastDecoder =
    Decode.succeed DailyForecast
        |> required "time" Decode.int
        |> required "summary" Decode.string
        |> required "icon" Decode.string
        |> required "precipIntensity" Decode.float
        |> required "precipProbability" Decode.float
        |> required "temperatureMin" Decode.float
        |> required "temperatureMax" Decode.float


completeForecastDecoder : Decode.Decoder CompleteForecast
completeForecastDecoder =
    Decode.succeed CompleteForecast
        |> required "latitude" Decode.float
        |> required "longitude" Decode.float
        |> required "timezone" Decode.string
        |> required "currently" forecastDecoder
        |> required "hourly" (timespanForecastDecoder hourlyForecastDecoder)
        |> required "daily" (timespanForecastDecoder dailyForecastDecoder)
