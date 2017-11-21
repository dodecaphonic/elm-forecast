module Forecast.Location exposing (Location)

import Forecast.DarkSky exposing (CompleteForecast)


type alias Location =
    { name : String
    , latitude : Float
    , longitude : Float
    , isSelected : Bool
    , currentForecast : Maybe CompleteForecast
    , id : Int
    }
