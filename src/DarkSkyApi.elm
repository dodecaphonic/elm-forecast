module Forecast.DarkSkyApi(queryForecast) where

import Effects exposing (Effects)
import Task exposing (Task, andThen)
import Http

import Forecast.Actions exposing (Action(..))
import Forecast.DarkSky exposing (completeForecastDecoder, CompleteForecast)
import Forecast.Location exposing (Location)


queryForecast : Location -> Effects Action
queryForecast loc =
  Http.get completeForecastDecoder (darkSky loc.latitude loc.longitude)
    |> Task.toMaybe
    |> Task.map UpdateForecast
    |> Effects.task


darkSky : Float -> Float -> String
darkSky lat lon =
  Debug.log "woot" ("http://localhost:9292/fetch/" ++ (toString lat) ++ "/" ++ (toString lon))
