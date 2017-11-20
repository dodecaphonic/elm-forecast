module Forecast.DarkSky
    exposing
        ( completeForecastDecoder
        , CompleteForecast
        , TimespanForecast
        , Forecast
        , DailyForecast
        , HourlyForecast
        )

import Json.Decode as Json
import Json.Decode.Pipeline exposing (decode, required)


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


timespanForecastDecoder : Json.Decoder ft -> Json.Decoder (TimespanForecast ft)
timespanForecastDecoder dataDecoder =
    decode TimespanForecast
        |> required "summary" Json.string
        |> required "icon" Json.string
        |> required "data" (Json.list dataDecoder)


hourlyForecastDecoder : Json.Decoder HourlyForecast
hourlyForecastDecoder =
    decode HourlyForecast
        |> required "time" Json.int
        |> required "summary" Json.string
        |> required "icon" Json.string
        |> required "temperature" Json.float
        |> required "precipIntensity" Json.float
        |> required "precipProbability" Json.float


forecastDecoder : Json.Decoder Forecast
forecastDecoder =
    decode Forecast
        |> required "time" Json.int
        |> required "summary" Json.string
        |> required "icon" Json.string
        |> required "precipIntensity" Json.float
        |> required "precipProbability" Json.float
        |> required "temperature" Json.float
        |> required "windSpeed" Json.float
        |> required "windBearing" Json.float
        |> required "humidity" Json.float
        |> required "visibility" Json.float
        |> required "cloudCover" Json.float
        |> required "pressure" Json.float
        |> required "ozone" Json.float


dailyForecastDecoder : Json.Decoder DailyForecast
dailyForecastDecoder =
    decode DailyForecast
        |> required "time" Json.int
        |> required "summary" Json.string
        |> required "icon" Json.string
        |> required "precipIntensity" Json.float
        |> required "precipProbability" Json.float
        |> required "temperatureMin" Json.float
        |> required "temperatureMax" Json.float


completeForecastDecoder : Json.Decoder CompleteForecast
completeForecastDecoder =
    decode CompleteForecast
        |> required "latitude" Json.float
        |> required "longitude" Json.float
        |> required "timezone" Json.string
        |> required "currently" forecastDecoder
        |> required "hourly" (timespanForecastDecoder hourlyForecastDecoder)
        |> required "daily" (timespanForecastDecoder dailyForecastDecoder)
