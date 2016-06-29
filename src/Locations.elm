module Forecast.Locations exposing (rio, london)

import Forecast.Location exposing (Location)


rio : Location
rio =
    { name = "Rio de Janeiro"
    , latitude = -22.9068
    , longitude = -43.1729
    , id = 1
    , isSelected = False
    }


london : Location
london =
    { name = "London"
    , latitude = 51.5072
    , longitude = -0.1275
    , id = 2
    , isSelected = False
    }
