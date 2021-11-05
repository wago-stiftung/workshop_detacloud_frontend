module Main exposing (main)

import Debug
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr
import Html.Events as HE exposing (onClick)
import Svg
import Svg.Attributes as SvgAttr
import Http
import Json.Decode as D exposing (Decoder)
import Dict exposing (Dict)
import Task
import Time

-- MODEL

type alias Lights =
    Dict String Bool

type alias Model =
    { url : String
    , count : Int
    , lights : Lights
    , zone : Time.Zone
    , time : Time.Posix
    }


type Msg
    = GetLights
    | Increment
    | Decrement
    | GotLights (Result Http.Error Lights)
    | AdjustTimeZone Time.Zone
    | Tick Time.Posix


initialModel : Model
initialModel =
    { url = ""
    , count = 0
    , lights = Dict.fromList
        [ ( "esstisch", False )
        , ( "couchtisch", False )
        , ( "gaestezimmer", False )
        , ( "schlafzimmer", False )
        , ( "bad1", False )
        , ( "bad2", False )
        , ( "bad3", False )
        , ( "garderobe", False )
        , ( "flur", False )
        , ( "kueche", False )
        ]
    , zone = Time.utc
    , time = (Time.millisToPosix 0)
    }


init : String -> (Model, Cmd Msg)
init url =
    ( { initialModel | url = url }
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , cmdGetLights url
        ]
    )

-- UPDATES

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetLights ->
            ( model, cmdGetLights model.url )

        Increment ->
            ( { model | count = model.count + 1 }
            , Cmd.none
            )

        Decrement ->
            ( { model | count = model.count - 1 }
            , Cmd.none
            )
        
        GotLights result ->
            case result of
                Ok l ->
                    ( { model | lights = l }
                    , cmdGetLights model.url
                    )
                Err _ ->
                    ( model
                    , cmdGetLights model.url
                    )
        
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )


-- VIEWS

view : Model -> Html Msg
view model =
    Html.main_
        []
        [ Svg.svg
            [ SvgAttr.id "floorplan"
            , SvgAttr.viewBox "0 0 156 110"
            ]
            [ Svg.defs []
                [ Svg.radialGradient
                    [ SvgAttr.id "rgrad"
                    , SvgAttr.cx "50%"
                    , SvgAttr.cy "50%"
                    , SvgAttr.r "25%"
                    ]
                    [ Svg.stop
                        [ SvgAttr.offset "0%"
                        , SvgAttr.style "stop-color:rgb(255,255,0);stop-opacity:0.70"
                        ]
                        []
                    , Svg.stop
                        [ SvgAttr.offset "100%"
                        , SvgAttr.style "stop-color:rgb(255,255,255);stop-opacity:0.00"
                        ]
                        []
                    ]
                ]
            , Svg.image
                [ SvgAttr.x "0"
                , SvgAttr.y "0"
                , SvgAttr.width "156"
                , SvgAttr.height "110"
                , Attr.attribute "href" "assets/floorplan.jpg"
                ]
                []
            , svgLight model "esstisch" "48" "92" "20"
            , svgLight model "couchtisch" "57" "51" "20"
            , svgLight model "gaestezimmer" "66" "18" "30"
            , svgLight model "schlafzimmer""110" "28" "30"
            , svgLight model "bad1" "142" "14" "20"
            , svgLight model "bad2" "142" "30" "20"
            , svgLight model "bad3" "142" "55" "20"
            , svgLight model "garderobe""140" "82" "20"
            , svgLight model "flur" "122" "82" "20"
            , svgLight model "kueche" "92" "90" "20"
            ]
        , button [ onClick GetLights ] [ text "Refresh" ]
        ]

svgLight model name cx cy r =
    Svg.circle
        [ if Maybe.withDefault False <| Dict.get name model.lights then
            SvgAttr.class "on"
          else
            SvgAttr.class "off"
        , SvgAttr.id name
        , SvgAttr.cx cx
        , SvgAttr.cy cy
        , SvgAttr.r r
        -- , SvgAttr.height height
        , SvgAttr.fill "url(#rgrad)"
        ]
        []

-- MAIN


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- COMMANDS


cmdGetLights : String -> Cmd Msg
cmdGetLights url =
    Http.get
        { url = url
        , expect = Http.expectJson GotLights (D.dict D.bool)
        }

