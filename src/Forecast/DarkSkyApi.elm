module Forecast.DarkSkyApi exposing (queryForecast)

import Platform.Cmd exposing (Cmd)
import Task exposing (Task, andThen)
import Http
import Forecast.Messages exposing (Msg(..))
import Forecast.DarkSky exposing (completeForecastDecoder, CompleteForecast)
import Forecast.Location exposing (Location)


queryForecast : Location -> Cmd Msg
queryForecast loc =
    Http.send (UpdateForecast loc) <|
        (Http.get (darkSky loc.latitude loc.longitude) completeForecastDecoder)


darkSky : Float -> Float -> String
darkSky lat lon =
    "http://localhost:9292/fetch/" ++ (toString lat) ++ "/" ++ (toString lon)
