module Forecast.DarkSky
    exposing
        ( completeForecastDecoder
        , CompleteForecast
        , TimespanForecast
        , Forecast
        , DailyForecast
        , HourlyForecast
        )

import Json.Decode as Json exposing (andThen, field, map2, succeed)


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


(:=) =
    field


(<*>) : Json.Decoder (a -> b) -> Json.Decoder a -> Json.Decoder b
(<*>) fab da =
    fab |> andThen (\ab -> andThen (\a -> succeed (ab a)) da)


timespanForecastDecoder : Json.Decoder ft -> Json.Decoder (TimespanForecast ft)
timespanForecastDecoder dataDecoder =
    Json.map3 TimespanForecast
        ("summary" := Json.string)
        ("icon" := Json.string)
        ("data" := Json.list dataDecoder)


hourlyForecastDecoder : Json.Decoder HourlyForecast
hourlyForecastDecoder =
    Json.map6 HourlyForecast
        ("time" := Json.int)
        ("summary" := Json.string)
        ("icon" := Json.string)
        ("temperature" := Json.float)
        ("precipIntensity" := Json.float)
        ("precipProbability" := Json.float)


forecastDecoder : Json.Decoder Forecast
forecastDecoder =
    (succeed Forecast)
        <*> ("time" := Json.int)
        <*> ("summary" := Json.string)
        <*> ("icon" := Json.string)
        <*> ("precipIntensity" := Json.float)
        <*> ("precipProbability" := Json.float)
        <*> ("temperature" := Json.float)
        <*> ("windSpeed" := Json.float)
        <*> ("windBearing" := Json.float)
        <*> ("humidity" := Json.float)
        <*> ("visibility" := Json.float)
        <*> ("cloudCover" := Json.float)
        <*> ("pressure" := Json.float)
        <*> ("ozone" := Json.float)


dailyForecastDecoder : Json.Decoder DailyForecast
dailyForecastDecoder =
    Json.map7 DailyForecast
        ("time" := Json.int)
        ("summary" := Json.string)
        ("icon" := Json.string)
        ("precipIntensity" := Json.float)
        ("precipProbability" := Json.float)
        ("temperatureMin" := Json.float)
        ("temperatureMax" := Json.float)


completeForecastDecoder : Json.Decoder CompleteForecast
completeForecastDecoder =
    Json.map6 CompleteForecast
        ("latitude" := Json.float)
        ("longitude" := Json.float)
        ("timezone" := Json.string)
        ("currently" := forecastDecoder)
        ("hourly" := (timespanForecastDecoder hourlyForecastDecoder))
        ("daily" := (timespanForecastDecoder dailyForecastDecoder))
