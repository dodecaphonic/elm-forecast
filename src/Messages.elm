module Forecast.Messages exposing (Msg(..))

import Http

import Forecast.Location exposing (Location)
import Forecast.Geocoding exposing (GeoLocation)
import Forecast.DarkSky as DS

type Msg = NoOp
         | SelectLocation Location
         | UpdateForecastFail Http.Error
         | UpdateForecastSucceed DS.CompleteForecast
         | GeocodeLocation String
         | ShowGeocodingOptions (List GeoLocation)
