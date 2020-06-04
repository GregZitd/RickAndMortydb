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
import Html
import Html.Attributes
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Element.Events as Events
import Element.Region as Region


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
    , windowSize : WindowSize
    , route : Route
    , searchBarContent : String
    , searchResult : SearchResult
    , lastRequestSent : String
    , searchBarFocused : Bool
    }

type alias Height = Int
type alias Width = Int

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
            , windowSize = WindowSize 0 0
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
                                , windowSize = WindowSize
                                                  flagsDecoded.width
                                                  flagsDecoded.height
                        }
               in ( initModel, Cmd.batch [ initCmdMsg, focusSearchBox ] )
               
           Err _ ->
               ( model, Cmd.none )

-- JS FLAGS

type alias WindowSize =
    { width : Int
    , height : Int
    }

setWindowHeightPc : Float -> WindowSize -> WindowSize
setWindowHeightPc perc window =
    let height = window.height
    in { window | height = percent perc height }

setWindowWidthPc : Float -> WindowSize -> WindowSize
setWindowWidthPc perc window =
    let width = window.width
    in { window | width = percent perc width }

setWindowHeightPx : Int -> WindowSize -> WindowSize
setWindowHeightPx size window =
    { window | height = size }

windowToDevice : Int -> Int -> Device
windowToDevice width height =
    classifyDevice
        { width = width
        , height = height
        }

setWindowWidthPx : Int -> WindowSize -> WindowSize
setWindowWidthPx size window =
    { window | width = size }


decodeFlags : D.Decoder WindowSize
decodeFlags =
    D.map2 WindowSize
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
    | GoToResultsPage Navigate
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
                      , windowSize = WindowSize width height
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

        GoToResultsPage navigate ->
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
                  ] <|
                     let topBarHeight = percent 7 model.windowSize.height
                     in [ viewTopBar (setWindowHeightPx topBarHeight model.windowSize)
                                     model.device
                                     model.route
                                     model.searchBarContent
                        , case model.route of
                              Home ->
                                  viewHomePage
                                      (setWindowHeightPx
                                           (model.windowSize.height - topBarHeight)
                                           model.windowSize)
                                      model.device
                                      model.searchBarContent
                              About ->
                                  viewAboutPage model
                              SearchResultsPage _ ->
                                  viewResultsPage
                                      (setWindowHeightPx
                                           (model.windowSize.height - topBarHeight)
                                           model.windowSize)
                                      model
                              NotFound ->
                                  text "page not found"
                              CharacterPage charId ->
                                  text <| "character Id: " ++ ( String.fromInt charId )
                        , viewFooter (setWindowHeightPc 1.5 model.windowSize)
                        ]
        ]
    }

viewTopBar : WindowSize -> Device -> Route -> String -> Element Msg
viewTopBar topBarSize device route searchBarContent=
    let shouldSearchBarShow =
            case route of
                SearchResultsPage _ ->
                    case device.class of
                        Phone ->
                            case device.orientation of
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
                      let searchBarHeight = percent 80 topBarSize.height
                          searchBarWidth = percent 30 topBarSize.width
                                             -- |> max 350
                                              --|> min (percent 50 topBarSize.width)
                      in viewTopBarSearch searchBarContent (WindowSize searchBarWidth searchBarHeight)
        
    in row [ Background.color white
           , width fill
           , height <| px topBarSize.height
           , paddingEach
                 { left = percent 8 topBarSize.width
                 , right = percent 2 topBarSize.width
                 , bottom = 0
                 , top = 0
                 }
           , Font.color black
           , spacing (percent 30 topBarSize.height)
           , Font.size (percent 43 topBarSize.height)
           , Region.navigation
           ]
           [ searchBar shouldSearchBarShow
           , viewTopBarButton "/" "Home"
           , viewTopBarButton "/About" "About"
           ]

viewTopBarSearch : String -> WindowSize -> Element Msg
viewTopBarSearch searchBarContent searchBarSize =
    let fontSize = percent 60 searchBarSize.height
    in row
        [ Background.color black
        , width <| px searchBarSize.width
        , height <| px searchBarSize.height
        , Border.rounded (percent 35 searchBarSize.height)
        , paddingEach
            { top = 0
            , right = percent 25 searchBarSize.height
            , bottom = 0
            , left = percent 20 searchBarSize.height
            }
        ]
        [ Input.search
              [ Background.color black
              , height fill
              , width fill
              , Font.color white
              , noFocusShadow
              , Border.width 0
              , padding <| ( searchBarSize.height - fontSize ) // 2
              , Font.size fontSize
              , Border.rounded (percent 50 searchBarSize.height)
              , Events.onFocus SearchBarGetsFocus
              , Events.onLoseFocus SearchBarLosesFocus
              ]
              { onChange = SearchBarChanged
              , text = searchBarContent
              , placeholder = Nothing
              , label = Input.labelHidden "Search input"
              }
        , el
            [ Background.uncropped "Images/blue_search_icon.png"
            , height <| px (percent 80 searchBarSize.height)
            , width <| px (percent 80 searchBarSize.height)
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

viewFooter : WindowSize -> Element Msg
viewFooter footerSize =
    paragraph
        [ Font.center
        , Font.color white
        , Font.size footerSize.height
        , paddingXY 0 (2 * footerSize.height)
        , alignBottom
        , Region.footer
        ] <|
        [ text "Developed by Gergely Malinoczki" ]

--VIEW RESULTS PAGE

viewResultsPage : WindowSize -> Model -> Element Msg
viewResultsPage resultsPageSize model =
    let currentPage =
            case getCurrentPage model.route of
                Just pageNum -> pageNum
                Nothing -> 0
        
        paddingCenter =
            paddingEach
               { top = percent 5 resultsPageSize.height
               , bottom = 0, left = 0, right = 0
               }
        paddingLeft = 
            paddingEach
               { top = percent 5 resultsPageSize.height
               , left = percent 10 resultsPageSize.width
               , bottom = 0, right = 0
               }
                                                
        resultsColumn : Width -> Orientation -> List Character -> Element Msg
        resultsColumn resultsColwidth orientation charList =
           column
              [ 
               spacing (percent 2 resultsPageSize.height)
              , Region.mainContent
              --, explain Debug.todo
              ] <|
                  let resultSize =
                          { width = resultsColwidth
                          , height = percent 35 resultsColwidth
                          }
                         
                  in case orientation of
                         Landscape ->
                             List.map
                                 ( viewCharacterResultHorizontal resultSize )
                                 charList
                         Portrait ->
                             List.map
                                 ( viewCharacterResultVertical resultSize.width )
                                 charList
              
                                                                    
    in case model.searchResult of
           CharacterSearch charRequest ->
               case model.device.orientation of
                       Landscape ->
                           case model.device.class of
                               Phone ->
                                   el
                                     [ centerX
                                     , paddingCenter
                                     ] <|
                                       resultsColumn
                                          (percent 80 resultsPageSize.width)
                                          Landscape
                                          charRequest.results
                               _ ->
                                   row
                                     [ alignLeft
                                     , paddingLeft
                                     ]
                                     [ resultsColumn
                                           (percent 35 resultsPageSize.width)
                                           Landscape
                                           charRequest.results
                                     ]
                       Portrait ->
                           case model.device.class of
                               Phone ->
                                   el
                                     [ centerX
                                     , paddingCenter
                                     ] <|
                                       resultsColumn
                                           ( percent 80 resultsPageSize.width )
                                           Portrait
                                           charRequest.results
                               Tablet ->
                                   el
                                     [ centerX
                                     , paddingCenter
                                     ] <|
                                       resultsColumn
                                           ( percent 80 resultsPageSize.width )
                                           Landscape
                                           charRequest.results
                               _ ->
                                   row
                                     [ alignLeft
                                     , paddingLeft
                                     ]
                                     [ resultsColumn
                                           ( percent 35 resultsPageSize.width )
                                           Landscape
                                           charRequest.results
                                     ]
               
           _ -> text "Something went wrong tetya"
        
getCurrentPage : Route -> Maybe Int
getCurrentPage route =
    case route of
        SearchResultsPage parameters -> parameters.page
        _ -> Nothing
                   
viewCharacterResultHorizontal : WindowSize  -> Character -> Element Msg
viewCharacterResultHorizontal resultSize character =
    let resultHeight = 0
    in row
        [ Background.color grey
        , height <| px resultSize.height
        , width <| px resultSize.width
        , Border.rounded 20
        , centerX
        ] <| 
        [ el
          [ height <| px resultSize.height
          , width <| px resultSize.height
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
        ++ [viewCharacterInfo
                (setWindowWidthPx
                     (resultSize.width - resultSize.height)
                     resultSize
                )
                character]
    
viewCharacterResultVertical : Width -> Character -> Element Msg
viewCharacterResultVertical resultWidth character =
    column
         [ Background.color grey
         , width <| px resultWidth
         , Border.rounded 20
         , centerX
         ] <|
         [ el
             [ height <| px resultWidth
             , width <| px resultWidth
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
         

    
viewCharacterInfo : WindowSize -> Character -> Element Msg
viewCharacterInfo infoSize character =
    let viewSpecies species subType =
            case subType of
                "" -> species
                _ -> species ++ " - " ++ subType
        subInfo title value =
            column
                   [ spacing (percent 2 infoSize.height) ]
                   [ paragraph
                         [ Font.size (percent 55 mainFontSize)
                         , Font.color <| rgb255 211 211 211
                         , Region.heading 2
                         ] <|
                         [ text title ]
                   , paragraph
                         [ Region.heading 3
                         , Font.size (percent 70 mainFontSize)
                         ] <|
                         [ text value ]
                   ]
        mainFontSize = percent 15 infoSize.height
    in column
        [ padding (percent 5 infoSize.height)
        , spacing (percent 10 infoSize.height)
        , alignTop
        ]
        [ paragraph
              [ width <| px (percent 90 infoSize.width)
              ]
              [ link
                    [ Font.bold
                    , Font.size mainFontSize
                    , mouseOver [ Font.color green ]
                    , Region.heading 1
                    ]
                    { url = "character/" ++ ( String.fromInt character.id )
                    , label = text character.name
                    }
              ]
        , subInfo "Status:" (statusToString character.status)
        , subInfo "Species:" ( viewSpecies character.species character.subType )
              
        ]


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
                        { onPress = Just ( GoToResultsPage Prev )
                        , label = text "Prev"
                        }
                        
        nextButton =
            case info.next of
                Nothing ->
                    none
                Just _ ->
                    Input.button
                        prevNextAttribute
                        { onPress = Just ( GoToResultsPage Next )
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
                { onPress = Just ( GoToResultsPage <| PageNum num )
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
                  , Region.navigation
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
             
viewHomePage : WindowSize -> Device -> String -> Element Msg
viewHomePage homePageSize device searchBarContent =
    column
        [ width fill
        , height fill
        , spacing (percent 5 homePageSize.width)
        , padding (percent 5 homePageSize.width)
        , Region.mainContent
        ]
        [ let headerSize =
               case device.class of
                   Phone -> setWindowWidthPc 90 homePageSize
                   Tablet -> setWindowWidthPc 75 homePageSize
                   _ -> setWindowWidthPc 60 homePageSize
          in viewHeader headerSize
        , let searchBarWidth =
                  case device.class of
                      Phone -> percent 80 homePageSize.width
                      Tablet -> percent 55 homePageSize.width
                      _ -> percent 40 homePageSize.width
            in viewSearchBar
                ( homePageSize
                    |> setWindowWidthPx searchBarWidth
                    |> setWindowHeightPc 6
                )
                searchBarContent
        , let searchButtonWidth =
                  case device.class of
                      Phone -> percent 30 homePageSize.width
                      Tablet -> percent 20 homePageSize.width
                      _ -> percent 15 homePageSize.width
          in viewSearchButton
              ( homePageSize
                    |> setWindowWidthPx searchButtonWidth
                    |> setWindowHeightPc 5
              )
        ]

viewHeader :WindowSize -> Element Msg
viewHeader headerSize =
     el
       [ centerX
       , Region.heading 1
       ] <|
         image
             [ width <| px headerSize.width
             ]
             { src = "Images/header.jpeg"
             , description = "Rick and Morty header"
             }


viewSearchBar : WindowSize -> String -> Element Msg
viewSearchBar searchBarSize searchBarContent =
    let fontSize = percent 60 searchBarSize.height
    in row
        [ centerX
        , Background.color white
        , Border.rounded (percent 50 searchBarSize.height)
        , width <| px searchBarSize.width
        , height <| px searchBarSize.height 
        , paddingEach { top = 0
                      , right = percent 50 searchBarSize.height
                      , bottom = 0
                      , left = percent 25 searchBarSize.height
                      }
        ]
        [ el
            [ Background.uncropped "Images/search_icon.png"
            , width <| px (percent 80 searchBarSize.height)
            , height <| px (percent 80 searchBarSize.height)
            ] none
        , Input.search
            [ Font.color black
            , Font.size fontSize
            , noFocusShadow
            , width fill
            , height fill
            , padding <| (searchBarSize.height - fontSize) // 2
            , Border.width 0
            , Events.onFocus SearchBarGetsFocus
            , Events.onLoseFocus SearchBarLosesFocus
            , htmlAttribute (Html.Attributes.id "home-page-searchbar")
            ]
            { onChange = SearchBarChanged
            , text = searchBarContent
            , placeholder = Nothing
            , label = Input.labelHidden "Search input"
            }
        ]

viewSearchButton : WindowSize -> Element Msg
viewSearchButton searchButtonSize =
    let offset = toFloat <| percent 6 searchButtonSize.height
    in Input.button
           [ Border.rounded (percent 50 searchButtonSize.height)
           , width <| px searchButtonSize.width
           , height <| px searchButtonSize.height
           , Background.color green
           , centerX
           , Font.center
           , Font.size (percent 50 searchButtonSize.height)
           , Font.color black
           , Font.bold
           , noFocusShadow
           , Border.shadow
               { offset = (offset,offset)
               , size = max 1 (offset / 2)
               , blur = 0
               , color = rgb255 0 150 150
               }
           , focused
                 [ moveRight offset
                 , moveDown offset
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

percent : Float -> Int -> Int
percent perc num =
    round <| perc * (toFloat num) / 100


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
