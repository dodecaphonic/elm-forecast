module Forecast.Geocoding exposing (GeoLocation)

import Http
import Task exposing (Task, andThen)
import Json.Decode as Json
import Json.Decode.Pipeline exposing (decode, required)


type alias Address =
    String


type alias GeoLocation =
    { formattedAddress : String
    , latitude : Float
    , longitude : Float
    }


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
    Json.field "results" (Json.list geolocationDecoder)


geolocationDecoder : Json.Decoder GeoLocation
geolocationDecoder =
    decode GeoLocation
        |> required "formatted_address" Json.string
        |> required "geometry" (Json.at [ "geometry", "location", "latitude" ] Json.float)
        |> required "geometry" (Json.at [ "geometry", "location", "longitude" ] Json.float)
