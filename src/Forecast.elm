port module Forecast exposing (..)

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


type alias Model =
    { locations : List Location
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


splitBy : (a -> Bool) -> List a -> ( List a, List a )
splitBy f xs =
    let
        updateRight =
            \( ls, rs ) x -> ( ls, rs ++ [ x ] )

        updateLeft =
            \( ls, rs ) x -> ( ls ++ [ x ], rs )

        split =
            List.foldl
                (\x ( upfn, ( ls, rs ) ) ->
                    if f x then
                        ( updateRight, ( ls, rs ) )
                    else
                        ( upfn, upfn ( ls, rs ) x )
                )
                ( updateLeft, ( [], [] ) )
                xs
    in
        Tuple.second split


updateSelectedLocationForecast : Model -> DS.CompleteForecast -> Model
updateSelectedLocationForecast model cf =
    let
        selectedLocation =
            model.locations
                |> List.filter .isSelected
                |> List.head

        updateForecast loc =
            let
                ( ls, rs ) =
                    splitBy (\l -> l == loc) model.locations

                withForecast =
                    { loc | currentForecast = Just cf }
            in
                ls ++ [ withForecast ] ++ rs

        locations =
            selectedLocation
                |> Maybe.map updateForecast
                |> Maybe.withDefault model.locations
    in
        { model
            | locations = locations
            , fetchingCurrentForecast = False
        }


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
                , queryForecast location
                )

        UpdateForecast (Ok cf) ->
            ( updateSelectedLocationForecast model cf
            , Cmd.none
            )

        UpdateForecast (Result.Err _) ->
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
        [ onClick (SelectLocation location)
        , css
            [ height (px 170)
            , backgroundColor (rgb 224 242 255)
            , borderRadius2 (px 5) (px 5)
            , cursor pointer
            , color (rgb 0 0 0)
            ]
        ]
        [ div
            [ css
                [ height (px 140)
                , margin2 (px 10) (px 10)
                ]
            ]
            [ div
                [ css
                    [ fontSize (Css.em 1.2) ]
                ]
                [ text location.name ]
            ]
        , div
            [ css
                [ height (px 20)
                , backgroundColor
                    (if location.isSelected then
                        (rgb 116 12 232)
                     else
                        (rgb 224 242 255)
                    )
                ]
            ]
            []
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
            W.forecast location forecast


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
        [ onClick (AddLocation location) ]
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
            ++ (List.map locationItem model.locations)
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
        [ Sub.map GeocodingSpinnerMsg Spinner.subscription
        , Sub.map ForecastSpinnerMsg Spinner.subscription
        ]


main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = subscriptions
        }
