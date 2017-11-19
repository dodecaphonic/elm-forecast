module Forecast.Messages exposing (Msg(..))

import Http
import Forecast.Location exposing (Location)
import Forecast.Geocoding exposing (GeoLocation)
import Forecast.DarkSky as DS


type alias KeyCode =
    Int


type Msg
    = NoOp
    | SelectLocation Location
    | UpdateForecast (Result Http.Error DS.CompleteForecast)
    | UpdateGeocodingLocation String
    | MaybeGeocodeLocation KeyCode
    | ShowGeocodingOptions (Result Http.Error (List GeoLocation))
    | AddLocation GeoLocation
