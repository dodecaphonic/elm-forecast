port module Forecast exposing (main, storeLocations)

import Browser
import Css exposing (..)
import Forecast.DarkSky as DS
import Forecast.Geocoding exposing (GeoLocation, fetchGeocoding)
import Forecast.Location exposing (Location)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, type_, value)
import Html.Styled.Events exposing (keyCode, on, onClick, onInput)
import Http
import Json.Decode as Json
import Platform.Cmd exposing (Cmd)
import Spinner
import Time exposing (every)


type alias KeyCode =
    Int


type Msg
    = NoOp
    | SelectLocation Location
    | UpdateForecast Location (Result Http.Error DS.CompleteForecast)
    | UpdateGeocodingLocation String
    | MaybeGeocodeLocation KeyCode
    | ShowGeocodingOptions (Result Http.Error (List GeoLocation))
    | AddLocation GeoLocation
    | GeocodingSpinnerMsg Spinner.Msg
    | ForecastSpinnerMsg Spinner.Msg
    | RefreshAllForecasts


queryForecast : Location -> Cmd Msg
queryForecast loc =
    Http.send (UpdateForecast loc) <|
        Http.get (darkSky loc.latitude loc.longitude) DS.completeForecastDecoder


darkSky : Float -> Float -> String
darkSky lat lon =
    "http://localhost:9292/fetch/" ++ String.fromFloat lat ++ "/" ++ String.fromFloat lon


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
                    { l | isSelected = True }
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
            List.map (\l -> { l | isSelected = False }) locations
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
                        { l | currentForecast = Just cf } :: ls

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
            forecastView location forecast


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
            ++ List.map locationListItem model.locations
        )


forecastView : Location -> DS.CompleteForecast -> Html Msg
forecastView location forecast =
    div
        []
        [ currently location forecast.currently
        , hourly forecast.hourly
        ]


forecastDivStyle : List Style
forecastDivStyle =
    [ displayFlex
    , flexDirection column
    , alignItems center
    , justifyContent center
    , borderRadius2 (px 5) (px 5)
    ]


currently : Location -> DS.Forecast -> Html Msg
currently location forecast =
    div
        [ css
            (temperatureStyles forecast.temperature
                ++ forecastDivStyle
            )
        ]
        [ summary location forecast
        , details forecast
        ]


hourly : DS.TimespanForecast DS.HourlyForecast -> Html Msg
hourly ts =
    div [] []


summary : Location -> DS.Forecast -> Html Msg
summary location forecast =
    div
        [ css
            [ width (pct 100)
            , displayFlex
            , alignItems center
            , marginTop (px 10)
            ]
        ]
        [ weatherIcon [] forecast.icon
        , span
            [ css
                [ flex (int 2)
                , marginLeft (px 15)
                ]
            ]
            [ text location.name ]
        , span
            [ css
                [ textAlign right
                , fontSize (Css.em 2.4)
                , fontWeight (int 500)
                , marginRight (px 15)
                ]
            ]
            [ text (formatTemperature forecast.temperature)
            ]
        ]


details : DS.Forecast -> Html Msg
details forecast =
    div
        [ css
            [ displayFlex
            , margin2 (px 30) (px 30)
            , width (pct 100)
            ]
        ]
        [ wind forecast.windSpeed forecast.windBearing
        , precipitation forecast.precipIntensity forecast.precipProbability
        , pressure forecast.pressure
        ]


wind : Float -> Float -> Html Msg
wind speed bearing =
    let
        icon =
            windBearing bearing

        title =
            String.fromFloat speed ++ " km/h"

        subtitle =
            bearingDescription bearing
    in
    dataPoint icon title subtitle


bearingDescription : Float -> String
bearingDescription bearing =
    if bearing == 0 || bearing == 360 then
        "North"

    else if bearing > 0 && bearing < 90 then
        "North East"

    else if bearing == 90 then
        "North East"

    else if bearing > 90 && bearing < 180 then
        "South East"

    else if bearing == 180 then
        "South"

    else if bearing > 180 && bearing < 270 then
        "South West"

    else if bearing == 270 then
        "West"

    else
        "North West"


windBearing : Float -> String
windBearing bearing =
    let
        icon =
            if bearing == 0 || bearing == 360 then
                "wi-wind-default _0-deg"

            else if bearing > 0 && bearing < 90 then
                "wi-wind-default _15-deg"

            else if bearing == 90 then
                "wi-wind-default _90-deg"

            else if bearing > 90 && bearing < 180 then
                "wi-wind-default _120-deg"

            else if bearing == 180 then
                "wi-wind-default _180-deg"

            else if bearing > 180 && bearing < 270 then
                "wi-wind-default _210-deg"

            else if bearing == 270 then
                "wi-wind-default _270-deg"

            else
                "wi-wind-default _345-deg"
    in
    "wi " ++ icon


precipitation : Float -> Float -> Html Msg
precipitation intensity probability =
    let
        icon =
            "wi wi-umbrella"

        title =
            String.fromFloat probability ++ "%"

        subtitle =
            String.fromFloat intensity ++ " cm"
    in
    dataPoint icon title subtitle


pressure : Float -> Html Msg
pressure press =
    let
        icon =
            "wi wi-down"

        title =
            String.fromFloat press ++ " mb"

        subtitle =
            "Pressure"
    in
    dataPoint icon title subtitle


summaryIcon : String -> String
summaryIcon iconName =
    case iconName of
        "clear-day" ->
            "wi-day-sunny"

        "clear-night" ->
            "wi-night-clear"

        "rain" ->
            "wi-rain"

        "snow" ->
            "wi-snow"

        "sleet" ->
            "wi-sleet"

        "wind" ->
            "wi-day-windy"

        "fog" ->
            "wi-day-fog"

        "cloudy" ->
            "wi-cloudy"

        "partly-cloudy-day" ->
            "wi-cloudy"

        "partly-cloudy-night" ->
            "wi-cloudy"

        otherwise ->
            "wi-sunset"


dataPoint : String -> String -> String -> Html Msg
dataPoint iconClass title subtitle =
    div
        [ css
            [ displayFlex
            , flex2 (int 1) (int 1)
            , fontSize (Css.em 0.9)
            ]
        ]
        [ div
            [ css
                [ fontSize (Css.em 1.5) ]
            ]
            [ i [ class iconClass ] [] ]
        , div
            [ css
                [ marginLeft (px 10) ]
            ]
            [ span
                [ css
                    [ display block ]
                ]
                [ text title ]
            , span
                [ css
                    [ fontWeight (int 300) ]
                ]
                [ text subtitle ]
            ]
        ]


temperatureStyles : Float -> List Style
temperatureStyles temp =
    if temp <= 0 then
        [ backgroundColor (hex "#FAFAFA")
        , color (hex "#000000")
        ]

    else if temp > 0 && temp <= 15 then
        [ backgroundColor (hex "#AFDCD8")
        , color (hex "#000000")
        ]

    else if temp > 15 && temp < 30 then
        [ backgroundColor (hex "#ECC055") ]

    else
        [ backgroundColor (hex "#E9B96F") ]


formatTemperature : Float -> String
formatTemperature temp =
    (String.fromInt <| truncate temp) ++ "º"


weatherIcon : List Style -> String -> Html msg
weatherIcon styles icon =
    i
        [ class ("wi " ++ summaryIcon icon)
        , css <|
            [ fontSize (Css.em 2.4)
            , marginLeft (px 15)
            ]
                ++ styles
        ]
        []


locationListItem : Location -> Html Msg
locationListItem location =
    let
        defaultColors =
            [ backgroundColor (rgb 224 242 255)
            , color (rgb 0 0 0)
            ]

        currentTemperature =
            .temperature << .currently

        colors =
            location.currentForecast
                |> Maybe.map (temperatureStyles << currentTemperature)
                |> Maybe.withDefault defaultColors

        temperatureValue =
            location.currentForecast
                |> Maybe.map (formatTemperature << currentTemperature)
                |> Maybe.withDefault "--"

        temperatureIcon =
            location.currentForecast
                |> Maybe.map (weatherIcon [ flex (int 1) ] << .icon << .currently)
                |> Maybe.withDefault (weatherIcon [ flex (int 1) ] "fog")
    in
    div
        [ onClick (SelectLocation location)
        , css <|
            [ height (px 120)
            , borderRadius2 (px 5) (px 5)
            , cursor pointer
            , marginBottom (px 20)
            ]
                ++ colors
        ]
        [ div
            [ css
                [ height (px 20)
                , backgroundColor
                    (if location.isSelected then
                        rgb 116 12 232

                     else
                        rgb 224 242 255
                    )
                ]
            ]
            []
        , div
            [ css
                [ margin2 (px 10) (px 10)
                , displayFlex
                , alignItems center
                , fontSize (Css.em 1.2)
                ]
            ]
            [ temperatureIcon
            , div
                [ css
                    [ alignSelf flexEnd
                    , fontSize (Css.em 1.4)
                    , fontWeight (int 500)
                    ]
                ]
                [ text temperatureValue ]
            ]
        , div
            [ css
                [ displayFlex
                , margin2 (px 10) (px 10)
                ]
            ]
            [ text location.name ]
        ]


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
