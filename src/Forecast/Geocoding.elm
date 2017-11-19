module Forecast.Geocoding exposing (GeoLocation)

import Http
import Task exposing (Task, andThen)
import Json.Decode as Json exposing (field)


type alias Address =
    String


type alias GeoLocation =
    { formattedAddress : String
    , latitude : Float
    , longitude : Float
    }


(:=) =
    field


fetchGeocoding : String -> Http.Request (List GeoLocation)
fetchGeocoding address =
    Debug.crash "update to elm 0.18"



-- if address == "" then
--     Task.succeed []
-- else
--     Http.get (googleGeocoder address) geocodingOptionsDecoder


googleGeocoder : String -> String
googleGeocoder address =
    -- Http.url "http://localhost:9292/geocode" [ ( "address", address ) ]
    Debug.crash "update to elm 0.18"


geocodingOptionsDecoder : Json.Decoder (List GeoLocation)
geocodingOptionsDecoder =
    ("results" := Json.list geolocationDecoder)


geolocationDecoder : Json.Decoder GeoLocation
geolocationDecoder =
    Json.map3 GeoLocation
        ("formatted_address" := Json.string)
        (Json.at [ "geometry", "location", "latitude" ] Json.float)
        (Json.at [ "geometry", "location", "longitude" ] Json.float)
