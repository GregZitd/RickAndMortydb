module Main exposing (..)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Browser.Dom as Dom
import Task
import Http
import Url
import Url.Parser as UP exposing ((<?>), (</>))
import Url.Parser.Query as Query
import Json.Decode as D
import Json.Encode as E
import Json.Decode.Pipeline as DP exposing (required, optional)
import Html exposing (Html)
import Html.Attributes
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

-- ROUTE PARSING

type Route =
      Home
    | About
    | NotFound
    | SearchResultsPage SearchResultsPageParameters
    | CharacterPage Int
      
type alias SearchResultsPageParameters =
    { charname : Maybe String
    , page : Maybe Int
    }
    
routeParser : UP.Parser (Route -> a) a
routeParser =
    UP.oneOf
        [ UP.map Home UP.top
        , UP.map About <| UP.s "About"
        , UP.map SearchResultsPage
            <|  UP.s "search" <?>
                   ( Query.map2 SearchResultsPageParameters
                                (Query.string "charname" )
                                ( Query.int "page" ) )
        , UP.map CharacterPage <| UP.s "character" </> UP.int
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
               let (initModel,initCmdMsg) =
                     handleUrlChange
                        { model | device = windowToDevice
                                               flagsDecoded.width
                                               flagsDecoded.height
                                , windowSize = Flags
                                                  flagsDecoded.width
                                                  flagsDecoded.height
                        }
               in ( initModel, Cmd.batch [ initCmdMsg, focusSearchBox ] )
               
           Err _ ->
               ( model, Cmd.none )

-- JS FLAGS

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
    | GotCharacterSearchResult String (Result Http.Error CharacterRequest)
    | GotSingleCharacter (Result Http.Error Character)
    | SearchBarGetsFocus
    | SearchBarLosesFocus
    | KeyPress Key
    | GoToPage Navigate
    | NoOp
      
type SearchResult =
      Failure Http.Error
    | Loading
    | CharacterSearch CharacterRequest
    | SingleCharacter Character
    | NoSearchInitiated

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UrlChange url ->
            let newModel =
                    { model | url = url
                    , route = toRoute url
                    }
            in handleUrlChange newModel
                                               
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
    
        GotCharacterSearchResult responseTo result ->
            case responseTo == model.lastRequestSent of
                False ->
                    ( model, Cmd.none )
                True ->
                    case result of
                        Ok characterList ->
                            ( { model | searchResult = CharacterSearch characterList }
                              ,Cmd.none
                            )
                        Err err ->
                            ( { model | searchResult = Failure err }
                            , Cmd.none
                            )

        GotSingleCharacter result ->
            case result of
                Ok character ->
                    ( { model | searchResult = SingleCharacter character }
                    , Cmd.none
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

        GoToPage navigate ->
            let newPageNum =
                    case model.route of
                        SearchResultsPage parameters ->
                            case navigate of
                                Next ->
                                    ( Maybe.withDefault 0 parameters.page ) + 1
                                Prev ->
                                    (Maybe.withDefault 0 parameters.page ) - 1
                                PageNum num ->
                                    num
                                    --This 1 here is just a dummy value. Should change it in the future for something that makes sense!!!
                        _ -> 1
            in case model.route of
                   SearchResultsPage parameters ->
                       ( model, Cmd.batch
                                    [ Nav.pushUrl model.key
                                            ( searchParametersToString
                                                  { parameters |
                                                        page = Just newPageNum
                                                  }
                                            )
                                    , resetViewport
                                    ]
                       )
                   _ ->
                       ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )
                           



searchParametersToString : SearchResultsPageParameters -> String
searchParametersToString parameters =
    let page = Maybe.withDefault 0 parameters.page
        charname = Maybe.withDefault "" parameters.charname
    in "/search?charname=" ++ charname ++ "&page=" ++ ( String.fromInt page )

            
searchButtonPressed : Model -> (Model,Cmd Msg)
searchButtonPressed model =
     case model.searchBarContent of
             "" -> ( model, Cmd.none )
             _ -> ( model, Nav.pushUrl
                                model.key
                                ( "/search?charname="
                                  ++ model.searchBarContent
                                  ++ "&page=1" ) )

handleUrlChange : Model -> (Model,Cmd Msg)
handleUrlChange model  =
     case model.route of
                   SearchResultsPage parameters ->
                       case parameters.charname of

                           Just searchTerm ->
                               case parameters.page of
                                   Just page ->
                                       getCharacterSearch searchTerm page model

                                   Nothing ->
                                       ( { model | route = NotFound }
                                       , Cmd.none
                                       )

                           Nothing ->
                               ( { model | route = NotFound }
                               , Cmd.none
                               )

                   Home ->
                       ( model, focusSearchBox )

                   CharacterPage charId ->
                       ( model, getSingleCaracter charId )
                           
                   _ ->
                       ( model, Cmd.none )

focusSearchBox : Cmd Msg
focusSearchBox =
    Task.attempt (\_ -> NoOp) (Dom.focus "home-page-searchbar")

resetViewport : Cmd Msg
resetViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 0 0)

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
                        SearchResultsPage _ ->
                            viewResultsPage model
                        NotFound ->
                            text "page not found"
                        CharacterPage charId ->
                            text <| "character Id: " ++ ( String.fromInt charId )
                  , viewFooter
                  ]
        ]
    }

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

viewFooter : Element Msg
viewFooter =
    paragraph
        [ Font.center
        , Font.color white
        , Font.size 12
        , paddingXY 0 30
        , alignBottom
        ] <|
        [ text "Developed by Gergely Malinoczki" ]

--VIEW RESULTS PAGE

viewResultsPage : Model -> Element Msg
viewResultsPage model =
    let currentPage =
            case getCurrentPage model.route of
                Just pageNum -> pageNum
                Nothing -> 0
    in case model.searchResult of
           CharacterSearch charRequest ->
               column
                   [ centerX
                   , padding 10
                   , spacing 5
                   ] <|
                   List.map ( viewCharacterResult model.device ) charRequest.results
                   ++ [ viewSearchPageNavigation
                            currentPage charRequest.info model.device]
           _ -> text "Something went wrong tetya"
        
getCurrentPage : Route -> Maybe Int
getCurrentPage route =
    case route of
        SearchResultsPage parameters -> parameters.page
        _ -> Nothing
                   
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
                            { url = "character/" ++ ( String.fromInt character.id )
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
                   , centerX
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
                  , centerX
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

viewSearchPageNavigation : Int -> RequestInfo -> Device -> Element Msg
viewSearchPageNavigation currentPageArg info device =
    let currentPageRadius =
            case device.class of
                Phone ->
                    case device.orientation of
                        Portrait -> 1
                        Landscape -> 2
                _ -> 3
                     
        showThesePageNums : Int-> Int -> List (Element Msg)
        showThesePageNums pages currentPage =
            if pages <= 5 then
                List.map pageNumberButton ( List.range 1 pages )
            else
                let lowerBound = max 1 (currentPage - currentPageRadius)
                    upperBound = min pages (currentPage + currentPageRadius)
                in     [ if lowerBound > 1 then text "..." else none ]
                    ++ List.map pageNumberButton ( List.range lowerBound upperBound )
                    ++ [ if upperBound < pages then text "..." else none ]
                        
        prevNextAttribute =
            [ Background.color green
            , Border.rounded 10
            , height <| px 30
            , width <| px 60
            , Font.color black
            , Font.center
            , Font.underline
            , noFocusShadow
            ]
            
        prevButton =
            case info.prev of
                Nothing ->
                    none
                Just _ ->
                    Input.button
                        prevNextAttribute
                        { onPress = Just ( GoToPage Prev )
                        , label = text "Prev"
                        }
                        
        nextButton =
            case info.next of
                Nothing ->
                    none
                Just _ ->
                    Input.button
                        prevNextAttribute
                        { onPress = Just ( GoToPage Next )
                        , label = text "Next"
                        }
                        
        pageNumberButton : Int -> Element Msg
        pageNumberButton num =
            Input.button
                [ Background.color green
                , Border.rounded 10
                , height <| px 30
                , padding 5
                , Font.color black
                , Font.center
                , noFocusShadow
                ]
                { onPress = Just ( GoToPage <| PageNum num )
                , label = text <| String.fromInt num
                }
                
    in case info.pages of
           1 ->
               --dont show navigation if there is only one page
               none
           _ ->
               row
                  [ centerX
                  , spacing 15
                  ] <|
                  [ prevButton ]
                  ++ showThesePageNums info.pages currentPageArg
                  ++ [ nextButton
                     ]

type Navigate =
      Next
    | Prev
    | PageNum Int

-- VIEW HOME PAGE
             
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
            , htmlAttribute (Html.Attributes.id "home-page-searchbar")
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

-- VIEW ABOUT PAGE

viewAboutPage : Model -> Element Msg
viewAboutPage model =
    column
        []
        [ text "About page"
        ]
       
-- PALETT

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

grey : Color
grey = rgb255 105 105 105




statusToString : Status -> String
statusToString status =
    case status of
        Alive -> "Alive"
        Dead -> "Dead"
        Unknown -> "unknown"
        InvalidStatus -> "Invalid status"
       
--HTTP

--initiates search and sets lastRequest to the search term to avoid http race conditions
getCharacterSearch : String -> Int -> Model -> ( Model, Cmd Msg )
getCharacterSearch searchTerm page model =
    let newModel = { model | lastRequestSent = searchTerm }
    in ( newModel
       , Http.get
           { url = "https://rickandmortyapi.com/api/character/?name="
                 ++ searchTerm
                 ++ "&page="
                 ++ ( String.fromInt page )
           , expect = Http.expectJson
                          (GotCharacterSearchResult searchTerm)
                              decodeCharacterRequest
           }
       )

getSingleCaracter : Int -> Cmd Msg
getSingleCaracter charId =
    Http.get
        { url = "https://rickandmortyapi.com/api/character/"
                ++ ( String.fromInt charId )
        , expect = Http.expectJson GotSingleCharacter decodeCharacter
        }

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
