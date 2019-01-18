module Forecast.Geocoding exposing (GeoLocation, fetchGeocoding)

import Http
import Task exposing (Task, andThen)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Url.Builder exposing (crossOrigin, string)


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
googleGeocoder address =
    crossOrigin "http://localhost:9292" [ "geocode" ] [ string "address" address ]


geocodingOptionsDecoder : Decode.Decoder (List GeoLocation)
geocodingOptionsDecoder =
    Decode.field "results" (Decode.list geolocationDecoder)


geolocationDecoder : Decode.Decoder GeoLocation
geolocationDecoder =
    Decode.succeed GeoLocation
        |> required "formatted_address" Decode.string
        |> required "address_components"
            (Decode.index 0 <| Decode.at [ "long_name" ] Decode.string)
        |> required "geometry" (Decode.at [ "location", "lat" ] Decode.float)
        |> required "geometry" (Decode.at [ "location", "lng" ] Decode.float)
