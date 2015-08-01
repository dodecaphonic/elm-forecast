module Forecast.DarkSkySignal(fetchForecast) where


import Task exposing (Task, andThen)
import Http

import Forecast.DarkSky exposing (completeForecastDecoder, CompleteForecast)


fetchForecast : Signal (Task Http.Error ())
fetchForecast =
  Signal.constant fetchOne
    |> Signal.map (\task -> task `andThen` Signal.send newForecast.address)


newForecast : Signal.Mailbox (Maybe CompleteForecast)
newForecast = Signal.mailbox Nothing


fetchOne : Task Http.Error (Maybe CompleteForecast)
fetchOne =
  Http.get completeForecastDecoder (darkSky -22.9068 -43.1729)


darkSky : Float -> Float -> String
darkSky lat lon =
  "http://localhost:9292/fetch/" ++ (toString lat) ++ "/" ++ (toString lon)
