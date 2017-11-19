port module Forecast exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Platform.Cmd exposing (Cmd)
import Task
import Http
import Json.Decode as Json
import Forecast.DarkSkyApi exposing (queryForecast)
import Forecast.Messages exposing (Msg(..))
import Forecast.Geocoding exposing (GeoLocation, fetchGeocoding)
import Forecast.Location exposing (Location)
import Forecast.DarkSky as DS
import Forecast.Widgets as W


type alias Model =
    { locations : List Location
    , currentForecast : Maybe DS.CompleteForecast
    , currentGeocodingOptions : List GeoLocation
    , geocodingInput : String
    , fetchingGeocoding : Bool
    }


init : List Location -> ( Model, Cmd Msg )
init locs =
    case locs of
        l :: ls ->
            let
                locations =
                    ({ l | isSelected = True }) :: ls
            in
                ( initialModel locations
                , queryForecast l
                )

        [] ->
            ( initialModel [], Cmd.none )


initialModel : List Location -> Model
initialModel locations =
    { locations = locations
    , currentForecast = Nothing
    , currentGeocodingOptions = []
    , geocodingInput = ""
    , fetchingGeocoding = False
    }


addLocation : Model -> GeoLocation -> ( Model, Cmd Msg )
addLocation model geolocation =
    let
        locations =
            model.locations

        newLocation =
            { name = geolocation.formattedAddress
            , latitude = geolocation.latitude
            , longitude = geolocation.longitude
            , isSelected = True
            , id = List.length locations + 1
            }

        newLocations =
            (List.map (\l -> { l | isSelected = False }) locations) ++ [ newLocation ]
    in
        ( { model | locations = newLocations, currentGeocodingOptions = [] }
        , Cmd.batch [ queryForecast newLocation, storeLocations newLocations ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelectLocation location ->
            let
                updateSelection loc =
                    { loc | isSelected = loc == location }
            in
                ( { model
                    | locations = List.map updateSelection model.locations
                    , currentForecast = Nothing
                  }
                , queryForecast location
                )

        UpdateForecast (Ok cf) ->
            ( { model | currentForecast = Just cf }, Cmd.none )

        UpdateForecast (Result.Err _) ->
            ( model, Cmd.none )

        ShowGeocodingOptions (Ok opts) ->
            ( { model
                | fetchingGeocoding = False
                , currentGeocodingOptions = opts
              }
            , Cmd.none
            )

        ShowGeocodingOptions (Result.Err _) ->
            ( { model | fetchingGeocoding = False }, Cmd.none )

        UpdateGeocodingLocation loc ->
            ( { model | geocodingInput = loc }, Cmd.none )

        MaybeGeocodeLocation key ->
            if key == 13 then
                ( { model | fetchingGeocoding = True }
                , Http.send ShowGeocodingOptions (fetchGeocoding model.geocodingInput)
                )
            else
                ( model, Cmd.none )

        AddLocation geolocation ->
            addLocation model geolocation



-- VIEW --


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)


locationItem : Location -> Html Msg
locationItem location =
    div
        [ classList [ ( "selected", location.isSelected ), ( "location", True ) ]
        , onClick (SelectLocation location)
        ]
        [ div [ class "data temp-warm" ]
            [ div [ class "place" ] [ text location.name ] ]
        ]


weatherView : Maybe Location -> Maybe DS.CompleteForecast -> Html Msg
weatherView location forecast =
    case location of
        Nothing ->
            noLocationSelected

        Just loc ->
            selectedLocation loc forecast


noLocationSelected : Html Msg
noLocationSelected =
    div [ class "forecast-container" ] [ text "Please select a location." ]


selectedLocation : Location -> Maybe DS.CompleteForecast -> Html Msg
selectedLocation location forecast =
    div [ class "forecast-container" ] [ (completeForecast location forecast) ]


completeForecast : Location -> Maybe DS.CompleteForecast -> Html Msg
completeForecast location cf =
    case cf of
        Nothing ->
            div [] [ text "Weather goes here." ]

        Just forecast ->
            W.forecast location forecast


addLocationInput : Model -> Html Msg
addLocationInput model =
    div [ class "add-location" ]
        [ input
            [ type_ "text"
            , value model.geocodingInput
            , onInput UpdateGeocodingLocation
            , onKeyUp MaybeGeocodeLocation
            ]
            []
        ]


geocodedLocationItem : GeoLocation -> Html Msg
geocodedLocationItem location =
    li
        [ onClick (AddLocation location) ]
        [ text location.formattedAddress ]


geocodedLocationsList : Model -> Html Msg
geocodedLocationsList model =
    ul [ class "geocoded-locations" ]
        (List.map geocodedLocationItem model.currentGeocodingOptions)


locationList : Model -> Html Msg
locationList model =
    div
        [ class "locations" ]
        ([ addLocationInput model
         , geocodedLocationsList model
         ]
            ++ (List.map locationItem model.locations)
        )


view : Model -> Html Msg
view model =
    let
        selectedLocation =
            model.locations
                |> List.filter .isSelected
                |> List.head
    in
        div [ class "container" ]
            [ locationList model
            , weatherView selectedLocation model.currentForecast
            ]



-- PORT --


port storeLocations : List Location -> Cmd msg


main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
