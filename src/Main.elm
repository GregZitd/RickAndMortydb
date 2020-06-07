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

import Palett exposing (..)
import ResultsPage exposing (Navigate(..), SearchResult(..),  viewResultsPage)
import Character exposing (..)


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
      -- , lastRequestSent : String
    , searchBarFocused : Bool
   -- , resultsPageModel : Maybe ResultsPage.Model
    --Result
    , searchResult : SearchResult
    , currentSearchTerm : String
    , currentPage : Int
    , currentCharacterOnShow : Maybe Character
    }


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
            , currentSearchTerm = ""
            , currentPage = 0
            , searchBarFocused = False
            , currentCharacterOnShow = Nothing
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


windowToDevice : Int -> Int -> Device
windowToDevice width height =
    classifyDevice
        { width = width
        , height = height
        }

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
    | ResultsPageMsg ResultsPage.Msg
    | NoOp
     

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
            case responseTo == model.currentSearchTerm of
                False ->
                    ( model, Cmd.none )
                True ->
                    case result of
                        Ok characterRequest ->
                            ( { model | searchResult = CharacterSearch characterRequest }
                            , Cmd.none
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

        ResultsPageMsg resultsMsg ->
            let (newModel, newMsg) =  ResultsPage.update resultsMsg model
            in (newModel, Cmd.map ResultsPageMsg newMsg)
        
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
                                       getCharacterSearch searchTerm 1 model
                                       --( { model | route = NotFound }
                                       --, Cmd.none
                                       

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
                                  Element.map ResultsPageMsg
                                      <| viewResultsPage
                                          (setWindowHeightPx
                                               (model.windowSize.height - topBarHeight)
                                               model.windowSize
                                          )
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
       
--HTTP

--initiates search and sets lastRequest to the search term to avoid http race conditions
getCharacterSearch : String -> Int -> Model -> ( Model, Cmd Msg )
getCharacterSearch searchTerm page model =
    let newModel = { model | currentSearchTerm = searchTerm
                           , currentPage = page
                   }
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



