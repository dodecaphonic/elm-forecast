module Forecast.Geocoding exposing (GeoLocation, fetchGeocoding)

import Http
import Task exposing (Task, andThen)
import Json.Decode as Json
import Json.Decode.Pipeline exposing (decode, required)


type alias Address =
    String


type alias GeoLocation =
    { formattedAddress : String
    , placeName : String
    , latitude : Float
    , longitude : Float
    }


fetchGeocoding : String -> Http.Request (List GeoLocation)
fetchGeocoding address =
    Http.get (googleGeocoder address) geocodingOptionsDecoder


googleGeocoder : String -> String
googleGeocoder =
    ((++) "http://localhost:9292/geocode?address=") << Http.encodeUri


geocodingOptionsDecoder : Json.Decoder (List GeoLocation)
geocodingOptionsDecoder =
    Json.field "results" (Json.list geolocationDecoder)


geolocationDecoder : Json.Decoder GeoLocation
geolocationDecoder =
    decode GeoLocation
        |> required "formatted_address" Json.string
        |> required "address_components"
            (Json.index 0 <| Json.at [ "long_name" ] Json.string)
        |> required "geometry" (Json.at [ "location", "lat" ] Json.float)
        |> required "geometry" (Json.at [ "location", "lng" ] Json.float)
