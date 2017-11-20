port module Forecast exposing (..)

import Forecast.DarkSky as DS
import Forecast.DarkSkyApi exposing (queryForecast)
import Forecast.Geocoding exposing (GeoLocation, fetchGeocoding)
import Forecast.Location exposing (Location)
import Forecast.Messages exposing (Msg(..))
import Forecast.Widgets as W
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Http
import Json.Decode as Json
import Platform.Cmd exposing (Cmd)
import Spinner


type alias Model =
    { locations : List Location
    , currentForecast : Maybe DS.CompleteForecast
    , currentGeocodingOptions : List GeoLocation
    , geocodingInput : String
    , fetchingGeocoding : Bool
    , fetchingCurrentForecast : Bool
    , geocodingSpinner : Spinner.Model
    , forecastSpinner : Spinner.Model
    }


init : List Location -> ( Model, Cmd Msg )
init locs =
    case locs of
        l :: ls ->
            let
                locations =
                    ({ l | isSelected = True })
                        :: List.map (\ol -> { ol | isSelected = False }) ls
            in
                ( initialModel locations True
                , queryForecast l
                )

        [] ->
            ( initialModel [] False, Cmd.none )


initialModel : List Location -> Bool -> Model
initialModel locations fetchingForecast =
    { locations = locations
    , currentForecast = Nothing
    , currentGeocodingOptions = []
    , geocodingInput = ""
    , fetchingGeocoding = False
    , fetchingCurrentForecast = fetchingForecast
    , geocodingSpinner = Spinner.init
    , forecastSpinner = Spinner.init
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
                    , fetchingCurrentForecast = True
                  }
                , queryForecast location
                )

        UpdateForecast (Ok cf) ->
            ( { model
                | currentForecast = Just cf
                , fetchingCurrentForecast = False
              }
            , Cmd.none
            )

        UpdateForecast (Result.Err _) ->
            ( { model
                | currentForecast = Nothing
                , fetchingCurrentForecast = False
              }
            , Cmd.none
            )

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

        GeocodingSpinnerMsg msg ->
            let
                spinnerModel =
                    Spinner.update msg model.geocodingSpinner
            in
                { model | geocodingSpinner = spinnerModel } ! []

        ForecastSpinnerMsg msg ->
            let
                spinnerModel =
                    Spinner.update msg model.forecastSpinner
            in
                { model | forecastSpinner = spinnerModel } ! []



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


weatherView : Model -> Html Msg
weatherView model =
    let
        currentLocation =
            model.locations
                |> List.filter .isSelected
                |> List.head
    in
        Maybe.map (selectedLocation model) currentLocation
            |> Maybe.withDefault noLocationSelected


noLocationSelected : Html Msg
noLocationSelected =
    div
        [ class "forecast-container" ]
        [ text "Please select a location." ]


selectedLocation : Model -> Location -> Html Msg
selectedLocation model location =
    div
        [ class "forecast-container" ]
        [ completeForecast model location ]


completeForecast : Model -> Location -> Html Msg
completeForecast model location =
    case model.currentForecast of
        Nothing ->
            div []
                [ if model.fetchingCurrentForecast then
                    Spinner.view Spinner.defaultConfig model.forecastSpinner
                  else
                    text "Weather goes here."
                ]

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


geocodingSpinner : Model -> Html Msg
geocodingSpinner model =
    if model.fetchingGeocoding then
        Spinner.view Spinner.defaultConfig model.geocodingSpinner
    else
        text ""


locationList : Model -> Html Msg
locationList model =
    div
        [ class "locations" ]
        ([ addLocationInput model
         , geocodingSpinner model
         , geocodedLocationsList model
         ]
            ++ (List.map locationItem model.locations)
        )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ locationList model
        , weatherView model
        ]



-- PORT --


port storeLocations : List Location -> Cmd msg



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GeocodingSpinnerMsg Spinner.subscription
        , Sub.map ForecastSpinnerMsg Spinner.subscription
        ]


main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
