module Forecast.DarkSkySignal(queryForecast, newForecast) where


import Task exposing (Task, andThen)
import Http

import Forecast.DarkSky exposing (completeForecastDecoder, CompleteForecast)
import Forecast.Location exposing (Location)


port fetchForecast : Signal (Task Http.Error ())
port fetchForecast =
  queryForecast.signal
    |> Signal.map fetchOne
    |> Signal.map (\task -> task `andThen` Signal.send newForecast.address)


newForecast : Signal.Mailbox (Maybe CompleteForecast)
newForecast = Signal.mailbox Nothing


queryForecast : Signal.Mailbox (Maybe Location)
queryForecast = Signal.mailbox Nothing


fetchOne : (Maybe Location) -> Task Http.Error (Maybe CompleteForecast)
fetchOne location =
  case location of
    Nothing ->
      Task.succeed Nothing

    Just loc ->
      Http.get completeForecastDecoder (darkSky loc.latitude loc.longitude)


darkSky : Float -> Float -> String
darkSky lat lon =
  "http://localhost:9292/fetch/" ++ (toString lat) ++ "/" ++ (toString lon)
