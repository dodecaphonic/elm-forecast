module Forecast.Messages exposing (Msg(..))

import Forecast.DarkSky as DS
import Forecast.Geocoding exposing (GeoLocation)
import Forecast.Location exposing (Location)
import Http
import Spinner


type alias KeyCode =
    Int


type Msg
    = NoOp
    | SelectLocation Location
    | UpdateForecast Location (Result Http.Error DS.CompleteForecast)
    | UpdateGeocodingLocation String
    | MaybeGeocodeLocation KeyCode
    | ShowGeocodingOptions (Result Http.Error (List GeoLocation))
    | AddLocation GeoLocation
    | GeocodingSpinnerMsg Spinner.Msg
    | ForecastSpinnerMsg Spinner.Msg
