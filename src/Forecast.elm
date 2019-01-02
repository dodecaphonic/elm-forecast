port module Forecast exposing (..)

import Browser
import Css exposing (..)
import Forecast.DarkSky as DS
import Forecast.DarkSkyApi exposing (queryForecast)
import Forecast.Geocoding exposing (GeoLocation, fetchGeocoding)
import Forecast.Location exposing (Location)
import Forecast.Messages exposing (Msg(..))
import Forecast.Widgets as W
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, value, type_)
import Html.Styled.Events exposing (keyCode, on, onClick, onInput)
import Http
import Json.Decode as Json
import Platform.Cmd exposing (Cmd)
import Spinner
import Time exposing (every)


type alias Model =
    { locations : List Location
    , currentGeocodingOptions : List GeoLocation
    , geocodingInput : String
    , fetchingGeocoding : Bool
    , fetchingCurrentForecast : Bool
    , geocodingSpinner : Spinner.Model
    , forecastSpinner : Spinner.Model
    }


minute : Float
minute =
    60 * 1000


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
                , Cmd.batch <| List.map queryForecast locations
                )

        [] ->
            ( initialModel [] False, Cmd.none )


initialModel : List Location -> Bool -> Model
initialModel locations fetchingForecast =
    { locations = locations
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
            { name = geolocation.placeName
            , latitude = geolocation.latitude
            , longitude = geolocation.longitude
            , isSelected = True
            , currentForecast = Nothing
            , id = List.length locations + 1
            }

        newLocations =
            (List.map (\l -> { l | isSelected = False }) locations)
                ++ [ newLocation ]
    in
        ( { model
            | locations = newLocations
            , geocodingInput = ""
            , currentGeocodingOptions = []
          }
        , Cmd.batch [ queryForecast newLocation, storeLocations newLocations ]
        )


updateLocationForecast : Model -> Location -> DS.CompleteForecast -> ( Model, Cmd Msg )
updateLocationForecast model loc cf =
    let
        locations =
            List.foldr
                (\l ls ->
                    if l.id == loc.id then
                        ({ l | currentForecast = Just cf }) :: ls
                    else
                        l :: ls
                )
                []
                model.locations
    in
        ( { model
            | locations = locations
            , fetchingCurrentForecast = False
          }
        , storeLocations locations
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
                    , fetchingCurrentForecast = True
                  }
                , Cmd.none
                )

        UpdateForecast loc (Ok cf) ->
            updateLocationForecast model loc cf

        UpdateForecast _ (Result.Err _) ->
            ( { model | fetchingCurrentForecast = False }
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

        GeocodingSpinnerMsg spinnerMsg ->
            let
                spinnerModel =
                    Spinner.update spinnerMsg model.geocodingSpinner
            in
                ( { model | geocodingSpinner = spinnerModel }, Cmd.none )

        ForecastSpinnerMsg spinnerMsg ->
            let
                spinnerModel =
                    Spinner.update spinnerMsg model.forecastSpinner
            in
                ( { model | forecastSpinner = spinnerModel }, Cmd.none )

        RefreshAllForecasts ->
            ( model, Cmd.batch <| List.map queryForecast model.locations )



-- VIEW --


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)


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


forecastContainerStyle : Attribute msg
forecastContainerStyle =
    css
        [ width (px 350)
        , marginLeft (px 20)
        ]


noLocationSelected : Html Msg
noLocationSelected =
    div
        [ forecastContainerStyle ]
        [ text "Please select a location." ]


selectedLocation : Model -> Location -> Html Msg
selectedLocation model location =
    div
        [ forecastContainerStyle ]
        [ completeForecast model location ]


completeForecast : Model -> Location -> Html Msg
completeForecast model location =
    case location.currentForecast of
        Nothing ->
            div []
                [ if model.fetchingCurrentForecast then
                    fromUnstyled <|
                        Spinner.view Spinner.defaultConfig model.forecastSpinner
                  else
                    text "Weather goes here."
                ]

        Just forecast ->
            W.forecastView location forecast


addLocationInput : Model -> Html Msg
addLocationInput model =
    div []
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
        [ onClick (AddLocation location)
        , css
            [ cursor pointer ]
        ]
        [ text location.formattedAddress ]


geocodedLocationsList : Model -> Html Msg
geocodedLocationsList model =
    ul []
        (List.map geocodedLocationItem model.currentGeocodingOptions)


geocodingSpinner : Model -> Html Msg
geocodingSpinner model =
    if model.fetchingGeocoding then
        fromUnstyled <| Spinner.view Spinner.defaultConfig model.geocodingSpinner
    else
        text ""


locationList : Model -> Html Msg
locationList model =
    div
        [ css [ width (px 300) ] ]
        ([ addLocationInput model
         , geocodingSpinner model
         , geocodedLocationsList model
         ]
            ++ (List.map W.locationListItem model.locations)
        )


view : Model -> Html Msg
view model =
    div
        [ css
            [ displayFlex
            , flexDirection row
            ]
        ]
        [ locationList model
        , weatherView model
        ]



-- PORT --


port storeLocations : List Location -> Cmd msg



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ every
            (5 * minute)
            (\_ -> RefreshAllForecasts)
        ]


main =
    Browser.element
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = subscriptions
        }
