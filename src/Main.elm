module Main exposing (..)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Http
import Url
import Url.Parser as UP exposing ((<?>))
import Url.Parser.Query as Query
import Json.Decode as D
import Json.Encode as E
import Json.Decode.Pipeline as DP exposing (required, optional)
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Element.Events as Events


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
    , searchResult : SearchResult
    , lastRequestSent : String
    , searchBarFocused : Bool
    }

type alias Character =
    { id : Int
    , name : String
    , status : Status
    , species : String
    , subType : String
    , gender : String
    , origin : CharacterOriginLocation
    , location : CharacterOriginLocation
    , image : String
    , episode : List String
    , url : String
    }


type Status =
      Alive
    | Dead
    | Unknown
    | InvalidStatus

type Route =
      Home
    | About
    | NotFound
    | SearchResultsPage (Maybe String)
      

routeParser : UP.Parser (Route -> a) a
routeParser =
    UP.oneOf
        [ UP.map Home UP.top
        , UP.map About <| UP.s "About"
        , UP.map SearchResultsPage <| UP.s "search" <?> Query.string "charname"
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
            , searchResult = NoSearchInitiated
            , lastRequestSent = ""
            , searchBarFocused = False
            }

    in case D.decodeValue decodeFlags flags of
           Ok flagsDecoded ->
               initiateSearchFromUrl
                    { model | device = windowToDevice
                                           flagsDecoded.width
                                           flagsDecoded.height
                            , windowSize = Flags flagsDecoded.width flagsDecoded.height
                    }
               
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
    | SearchButtonPressed
    | GotSearchResult String (Result Http.Error CharacterRequest)
    | SearchBarGetsFocus
    | SearchBarLosesFocus
    | KeyPress Key
      
type SearchResult =
      Failure Http.Error
    | Loading
    | Result CharacterRequest
    | NoSearchInitiated

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UrlChange url ->
            let newModel =
                    { model | url = url
                    , route = toRoute url
                    }
            in initiateSearchFromUrl newModel
                                               
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

        SearchButtonPressed ->
            searchButtonPressed model
    
        GotSearchResult responseTo result ->
            case responseTo == model.lastRequestSent of
                False ->
                    ( model, Cmd.none )
                True ->
                    case result of
                        Ok characterList ->
                            ( { model | searchResult = Result characterList }
                              ,Cmd.none
                            )
                        Err err ->
                            ( { model | searchResult = Failure err }
                            , Cmd.none
                            )

        SearchBarGetsFocus ->
            ( { model | searchBarFocused = True }, Cmd.none )

        SearchBarLosesFocus ->
            ( { model | searchBarFocused = False }, Cmd.none )
                
        KeyPress key ->
            case key of
                Enter ->
                    case model.searchBarFocused of
                        False ->
                            ( model, Cmd.none )
                        True ->
                            searchButtonPressed model
                NonEnter ->
                    ( model, Cmd.none )


            
searchButtonPressed : Model -> (Model,Cmd Msg)
searchButtonPressed model =
     case model.searchBarContent of
             "" -> ( model, Cmd.none )
             _ -> ( model, Nav.pushUrl
                                model.key
                                ( "/search?charname=" ++ model.searchBarContent ) )

initiateSearchFromUrl : Model -> (Model,Cmd Msg)
initiateSearchFromUrl model  =
     case model.route of
                   SearchResultsPage searchUrl ->
                       case searchUrl of

                           Just searchTerm ->
                               getStuff searchTerm model

                           Nothing ->
                               ( { model | route = NotFound }
                               , Cmd.none
                               )

                   _ ->
                       ( model, Cmd.none )
--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize WindowResized
        , Browser.Events.onKeyPress keyDecoder
        ]

keyDecoder : D.Decoder Msg
keyDecoder =
    D.map toKey ( D.field "key" D.string )

toKey : String -> Msg
toKey keyValue =
    case keyValue of
        "Enter" -> KeyPress Enter
        _ -> KeyPress NonEnter
      
type Key =
      Enter
    | NonEnter

--VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Rick and Morty db"
    , body =
        [ layout
              [] <|
              column
                  [ Background.color black
                  , width fill
                  , height fill
                  , Font.color white
                  ]
                  [ viewTopBar model
                  , case model.route of
                        Home ->
                            viewHomePage model
                        About ->
                            viewAboutPage model
                        SearchResultsPage resultTo ->
                            viewResultsPage model
                        NotFound ->
                            text "page not found"
                  ]
        ]
    }

viewResultsPage : Model -> Element Msg
viewResultsPage model =
    case model.searchResult of
        Result charRequest ->
            column
                [ centerX
                , padding 10
                , spacing 5
                ] <|
                List.map ( viewCharacterResult model.device ) charRequest.results
        _ -> text "Something went wrong tetya"
        

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
        , viewSearchButton
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
        yPad = 0
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
            , height fill
            , padding <| (size.height - fontSize) // 2
            , Border.width 0
            , Events.onFocus SearchBarGetsFocus
            , Events.onLoseFocus SearchBarLosesFocus
            ]
            { onChange = SearchBarChanged
            , text = model.searchBarContent
            , placeholder = Nothing
            , label = Input.labelHidden "Search input"
            }
        ]

viewSearchButton : Element Msg
viewSearchButton =
    Input.button
        [ Border.rounded 20
        , width <| px 150
        , height <| px 40
        , Background.color green
        , centerX
        , Font.center
        , Font.size 20
        , Font.color black
        , Font.bold
        , noFocusShadow
        , Border.shadow
            { offset = (2,2)
            , size = 1
            , blur = 0
            , color = rgb255 0 150 150
            }
        , focused
              [ moveRight 2
              , moveDown 2
              , Border.shadow
                  { offset = (0,0)
                  , size = 0
                  , blur = 0
                  , color = green
                  }
              ]
        ]
        { onPress = Just SearchButtonPressed
        , label = text "Search"
        }

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
         
green : Color
green = rgb255 0 204 204

viewTopBar : Model -> Element Msg
viewTopBar model =
    let shouldSearchBarShow =
            case model.route of
                SearchResultsPage _ ->
                    case model.device.class of
                        Phone ->
                            case model.device.orientation of
                                Portrait -> False
                                Landscape -> True
                        _ -> True
                _ -> False
        searchBar : Bool -> Element Msg
        searchBar bool =
            case bool of
                False -> none
                True ->
                    el
                      [ alignLeft
                      ] <|
                      viewTopBarSearch model topBarHeight
        topBarHeight = 60
    in row [ Background.color white
           , width fill
           , height <| px topBarHeight
           , paddingXY 30 0
           , Font.color black
           , spacing 20
           ]
           [ searchBar shouldSearchBarShow
           , viewTopBarButton "/" "Home"
           , viewTopBarButton "/About" "About"
           ]

viewTopBarSearch : Model -> Int -> Element Msg
viewTopBarSearch model topBarHeight =
    let size =
            { width = 350
            , height = topBarHeight - 15
            }
        fontSize = 20
    in row
        [ Background.color black
        , width <| px size.width
        , height <| px size.height
        , Border.rounded 30
        , paddingEach
            { top = 0, right = 15, bottom = 0, left = 5 }
        , spacing 30
        ]
        [ Input.search
              [ Background.color black
              , height fill
              , width fill
              , Font.color white
              , noFocusShadow
              , Border.width 0
              --, explain Debug.todo
              , padding <| ( size.height - fontSize ) // 2
              , Font.size fontSize
              , Border.rounded 20
              , Events.onFocus SearchBarGetsFocus
              , Events.onLoseFocus SearchBarLosesFocus
              ]
              { onChange = SearchBarChanged
              , text = model.searchBarContent
              , placeholder = Nothing
              , label = Input.labelHidden "Search input"
              }
        , el
            [ Background.uncropped "Images/blue_search_icon.png"
            , height <| px (size.height - 15 )
            , width <| px ( size.height - 15 )
            , alignRight
            ] <|
            Input.button
                [ width fill
                , height fill
                , noFocusShadow
                ]
                { onPress = Just SearchButtonPressed
                , label = none
                }
        ]


viewTopBarButton : String -> String -> Element Msg
viewTopBarButton url label =
    link
      [ alignRight
      , Font.bold
      , mouseOver
            [ Font.color green
            ]
      ] <| { url = url
           , label = text label
           }
grey : Color
grey = rgb255 105 105 105

viewCharacterResultPC : Character -> Element Msg
viewCharacterResultPC character =
    let viewSpecies species subType =
            case subType of
                "" -> species
                _ -> species ++ " - " ++ subType
    in row
          [ Background.color grey
          , height <| px 150
          , width <| px 450
          , Border.rounded 20
          ]
          [ el
              [ height <| px 150
              , width <| px 150
              , Border.roundEach
                  { topLeft = 20
                  , topRight = 0
                  , bottomLeft = 20
                  , bottomRight = 0
                  }
              , Background.uncropped character.image
              ]
              none
          ,column
              [ padding 10
              , spacing 15
              , alignTop
              ]
              [ link
                   [ Font.bold
                   , mouseOver [ Font.color green ]
                   ]
                   { url = character.url
                   , label = text character.name
                   }
              , column
                  [ spacing 5]
                  [ el
                      [ Font.size 15
                      , Font.color <| rgb255 211 211 211
                      ] <|
                      text "Status:"
                  , el
                      [
                      ] <|
                      text ( statusToString character.status )
                  ]
              , column
                  [ spacing 5]
                  [ el
                      [ Font.size 15
                      , Font.color <| rgb255 211 211 211
                      ] <|
                      text "Species:"
                  , el
                      [
                      ] <|
                      text <| viewSpecies character.species character.subType
                  ]
              ]
          ]

viewCharacterResult : Device -> Character -> Element Msg
viewCharacterResult device character =
    let viewSpecies species subType =
            case subType of
                "" -> species
                _ -> species ++ " - " ++ subType

        textInfoPart =
            [ column
                  [ padding 10
                  , spacing 15
                  , alignTop
                  ]
                  [ paragraph []
                        [ link
                            [ Font.bold
                            , mouseOver [ Font.color green ]
                            ]
                            { url = character.url
                            , label = text character.name
                            }
                        ]
                  , column
                      [ spacing 5]
                      [ paragraph
                          [ Font.size 15
                          , Font.color <| rgb255 211 211 211
                          ] <|
                          [ text "Status:" ]
                      , paragraph
                          [] <|
                          [ text ( statusToString character.status ) ]
                      ]
                  , column
                      [ spacing 5]
                      [ paragraph
                          [ Font.size 15
                          , Font.color <| rgb255 211 211 211
                          , width <| px 200
                          ]
                          [ text "Species:" ]
                      , paragraph
                          [
                          ] <|
                          [ text <| viewSpecies character.species character.subType ]
                      ]
                  ]
            ]

        horizontalLook textInfo =
            row
                   [ Background.color grey
                   , height <| px 180
                   , width <| px 500
                   , Border.rounded 20
                   ] <| 
                   [ el
                       [ height <| px 180
                       , width <| px 180
                       , Border.roundEach
                           { topLeft = 20
                           , topRight = 0
                           , bottomLeft = 20
                           , bottomRight = 0
                           }
                       , Background.uncropped character.image
                       ]
                       none
                   ]
                   ++ textInfo

        verticalLook textInfo =
            column
                  [ Background.color grey
                  , width <| px 200
                  , Border.rounded 20
                  ] <|
                  [ el
                      [ height <| px 200
                      , width <| px 200
                      , Border.roundEach
                          { topLeft = 20
                          , topRight = 20
                          , bottomLeft = 0
                          , bottomRight = 0
                          }
                      , Background.uncropped character.image
                      ]
                      none
                  ]
                  ++ textInfo
                      
    in case device.class of
           Phone ->
               case device.orientation of
                   Portrait ->
                       verticalLook textInfoPart
                   Landscape ->
                       horizontalLook textInfoPart
                           
           _ ->
                horizontalLook textInfoPart
          
   
statusToString : Status -> String
statusToString status =
    case status of
        Alive -> "Alive"
        Dead -> "Dead"
        Unknown -> "unknown"
        InvalidStatus -> "Invalid status"

--HTTP

--initiates search and sets lastRequest to the search term to avoid http race conditions
getStuff : String -> Model -> ( Model, Cmd Msg )
getStuff searchTerm model =
    let newModel = { model | lastRequestSent = searchTerm }
    in ( newModel
       , Http.get
           { url = "https://rickandmortyapi.com/api/character/?name="
                 ++ searchTerm
           , expect = Http.expectJson
                          (GotSearchResult searchTerm)
                              decodeCharacterRequest
           }
       )

decodeCharacter : D.Decoder Character
decodeCharacter =
    D.succeed Character
        |> DP.required "id" D.int 
        |> DP.required "name" D.string
        |> DP.required "status" decodeStatus
        |> DP.required "species" D.string
        |> DP.required "type" D.string
        |> DP.required "gender" D.string
        |> DP.required "origin" decodeCharacterOriginLocation
        |> DP.required "location" decodeCharacterOriginLocation
        |> DP.required "image" D.string
        |> DP.required "episode" ( D.list D.string )
        |> DP.required "url" D.string
        
decodeStatus : D.Decoder Status
decodeStatus =
    let stringToStatus string =
            case string of
                "Alive" -> Alive
                "Dead" -> Dead
                "unknown" -> Unknown
                _ -> InvalidStatus
    in D.map stringToStatus D.string

decodeCharacterOriginLocation : D.Decoder CharacterOriginLocation
decodeCharacterOriginLocation =
    D.map2 CharacterOriginLocation
        ( D.field "name" D.string )
        ( D.field "url" D.string )
              

type alias CharacterOriginLocation =
    { name : String
    , url : String
    }

type alias CharacterRequest =
    { info : RequestInfo
    , results : List Character
    }

type alias RequestInfo =
    { count : Int
    , pages : Int
    , next : Maybe String
    , prev : Maybe String
    }

decodeRequestInfo : D.Decoder RequestInfo
decodeRequestInfo =
    D.succeed RequestInfo
        |> DP.required "count" D.int
        |> DP.required "pages" D.int
        |> DP.required "next" ( D.nullable D.string )
        |> DP.required "prev" ( D.nullable D.string )

decodeCharacterRequest : D.Decoder CharacterRequest
decodeCharacterRequest =
    D.map2 CharacterRequest
        ( D.field "info" decodeRequestInfo )
        ( D.field "results" ( D.list decodeCharacter ) )
