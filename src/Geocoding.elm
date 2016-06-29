module Forecast.Geocoding exposing (GeoLocation)

import Http
import Task exposing (Task, andThen)
import Json.Decode as Json exposing ((:=))


type alias Address =
    String


type alias GeoLocation =
    { formattedAddress : String
    , latitude : Float
    , longitude : Float
    }


fetchGeocoding : String -> Task Http.Error (List GeoLocation)
fetchGeocoding address =
    if address == "" then
        Task.succeed []
    else
        Http.get geocodingOptionsDecoder (googleGeocoder address)


googleGeocoder : String -> String
googleGeocoder address =
    Http.url "http://localhost:9292/geocode" [ ( "address", address ) ]


geocodingOptionsDecoder : Json.Decoder (List GeoLocation)
geocodingOptionsDecoder =
    ("results" := Json.list geolocationDecoder)


geolocationDecoder : Json.Decoder GeoLocation
geolocationDecoder =
    Json.object3 GeoLocation
        ("formatted_address" := Json.string)
        (Json.at [ "geometry", "location", "latitude" ] Json.float)
        (Json.at [ "geometry", "location", "longitude" ] Json.float)
