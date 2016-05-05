module Forecast.Actions(Action(..)) where

import Forecast.Location exposing (Location)
import Forecast.Geocoding exposing (GeoLocation)
import Forecast.DarkSky as DS

type Action = NoOp
            | SelectLocation Location
            | UpdateForecast (Maybe DS.CompleteForecast)
            | GeocodeLocation String
            | ShowGeocodingOptions (List GeoLocation)
