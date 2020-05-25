module Main exposing (..)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Http
import Url
import Url.Parser as UP
import Json.Decode as D
import Json.Encode as E
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border


--MAIN

main = Browser.application
       { init = init
       , update = update
       , subscriptions = subscriptions
       , view = view
       , onUrlChange = UrlChange
       , onUrlRequest = UrlRequest
       }

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , device : Device
    , windowSize : Flags
    , route : Route
    , searchBarContent : String
    }

type Route =
      Home
    | About
    | NotFound

routeParser : UP.Parser (Route -> a) a
routeParser =
    UP.oneOf
        [ UP.map Home UP.top
        , UP.map About <| UP.s "About"
        ]

toRoute : Url.Url -> Route
toRoute url  =
    Maybe.withDefault NotFound (UP.parse routeParser url)

init : D.Value -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
    let model =
            { key = key
            , url = url
            , device =
                { class = Tablet
                , orientation = Landscape
                }
            , windowSize = Flags 0 0
            , route = toRoute url
            , searchBarContent = ""
            }
    in case D.decodeValue decodeFlags flags of
           Ok flagsDecoded ->
               ( { model | device = windowToDevice
                                       flagsDecoded.width
                                       flagsDecoded.height
                 , windowSize = Flags flagsDecoded.width flagsDecoded.height
                 }
               , Cmd.none
               )

           Err _ ->
               ( model, Cmd.none )

type alias Flags =
    { width : Int
    , height : Int
    }

windowToDevice : Int -> Int -> Device
windowToDevice width height =
    classifyDevice
        { width = width
        , height = height
        }

decodeFlags : D.Decoder Flags
decodeFlags =
    D.map2 Flags
        (D.field "width" D.int)
        (D.field "height" D.int)
    

--UPDATE

type Msg =
      UrlChange Url.Url
    | UrlRequest Browser.UrlRequest
    | WindowResized Int Int
    | SearchBarChanged String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UrlChange url ->
            ( { model | url = url
                      , route = toRoute url}
            , Cmd.none
            )

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        WindowResized width height ->
            ( { model | device = windowToDevice width height
                      , windowSize = Flags width height
              }
            , Cmd.none
            )

        SearchBarChanged content ->
            ( { model | searchBarContent = content }
            , Cmd.none
            )

--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize WindowResized

--VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Rick and Morty db"
    , body =
        [ layout
              [ width fill
              , height fill
              ] <|
              column
                  [ Background.color black
                  , width fill
                  , height fill
                  , Font.color white
                  ]
                  [ viewTopBar
                  , case model.route of
                        Home ->
                            viewHomePage model
                        About ->
                            viewAboutPage model
                        NotFound ->
                            text "page not found"
                  ]
        ]
    }


viewHomePage : Model -> Element Msg
viewHomePage model =
    column
        [ width fill
        , height fill
        , spacing 50
        , padding 70
        ]
        [ viewHeader model
        , viewSearchBar model
        ]

viewAboutPage : Model -> Element Msg
viewAboutPage model =
    column
        []
        [ text "About page"
        ]
       
viewHeader : Model -> Element Msg
viewHeader model =
    let size =
         case model.device.class of
            Phone -> { width = 400, height = 150 }
            Tablet -> { width = 550, height = 185 }
            Desktop -> { width = 700, height = 230 }
            BigDesktop -> {width = 700, height = 230 }
    in el
         [ centerX
         , height <| px size.height
         , width <| maximum model.windowSize.width (px size.width)
         , Background.uncropped "Images/header.jpeg"
         ]
         none


viewSearchBar : Model -> Element Msg
viewSearchBar model =
    let size =
         case model.device.class of
             Phone -> { width = 400, height = 40 }
             Tablet -> { width = 500, height = 50 }
             _ -> { width = 600, height = 50 }
        yPad = 5
        spacingVal = 15
        fontSize = 25
    in row
        [ centerX
        , Background.color white
        , Border.rounded 30
        , width <| maximum (model.windowSize.width - 20) (px size.width)
        , height <| px size.height
        , paddingEach { top = yPad
                      , right = 30
                      , bottom = yPad
                      , left = 10
                      }
        , spacing spacingVal
        ]
        [ el
            [ Background.uncropped "Images/search_icon.png"
            , width <| px ( size.height - 15 )
            , height <| px ( size.height - 15 )
            ] none
        , Input.search
            [ Font.color black
            , Font.size fontSize
            , noFocusShadow
            , width fill
            , height <| px fontSize
            , padding 0
            , Border.width 0
            ]
            { onChange = SearchBarChanged
            , text = model.searchBarContent
            , placeholder = Nothing
            , label = Input.labelHidden "Search input"
            }
        ]

noFocusShadow : Attribute Msg
noFocusShadow =
    focused
        [ Border.shadow
              { offset = (0,0)
              , size = 0
              , blur = 0
              , color = white
                        }
        ]
    
black : Color
black = rgb255 0 0 0

white : Color
white = rgb255 255 255 255

orange : Color
orange = rgb255 255 140 0

viewTopBar : Element Msg
viewTopBar =
    row [ Background.color white
        , width fill
        , height <| px 60
        , paddingXY 30 0
        , Font.bold
        , Font.color black
        , spacing 20
        ]
        [ viewTopBarButton "/" "Home"
        , viewTopBarButton "/About" "About"
        ]

viewTopBarButton : String -> String -> Element Msg
viewTopBarButton url label =
    link
      [ alignRight
      , mouseOver
            [ Font.color orange
            ]
      ] <| { url = url
           , label = text label
           }
    
