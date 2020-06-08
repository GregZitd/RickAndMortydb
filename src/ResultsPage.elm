module ResultsPage exposing (..)

import Html
import Html.Attributes
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Element.Events as Events
import Element.Region as Region
import Browser.Navigation as Nav
import Browser.Dom as Dom
import Http
import Task
import Url.Builder as UB
import Animator as A
import Animator.Inline as AI
import Time


import Palett exposing (..)
import Character exposing (Character, Status(..), CharacterRequest,RequestInfo)

type alias Result a =
    { a | currentPage : Int
        , currentSearchTerm : String
        , searchResult : SearchResult
        , currentCharacterOnShow : Maybe Character
        , scrollPos : Int

        , key : Nav.Key
        , device : Device
    }

type SearchResult =
      Failure Http.Error
    | Loading
    | CharacterSearch CharacterRequest
    | SingleCharacter Character
    | NoSearchInitiated

type Msg =
      GoToResultsPage Navigate
    | ShowCharacterInfo Character
    | NoOp
    
update : Msg -> (Result model) -> (Result model, Cmd Msg)
update msg model =
    case model.searchResult of
        CharacterSearch charRequest ->
            case msg of
                GoToResultsPage navigate ->
                    case navigate of
                        
                        Prev ->
                            let cp = model.currentPage
                            in ( { model | currentPage = cp - 1 }
                               , case charRequest.info.prev of
                                     Just url ->
                                         Cmd.batch
                                             [ Nav.pushUrl
                                                   model.key
                                                   (modelToUrl
                                                        model.currentSearchTerm
                                                        ( model.currentPage - 1)
                                                   )
                                             , resetViewport
                                             ]
                                     Nothing -> Cmd.none
                               )
                                
                        Next ->
                            let cp = model.currentPage
                            in ( { model | currentPage = cp + 1 }
                               , case charRequest.info.next of
                                     Just url ->
                                         Cmd.batch
                                             [ Nav.pushUrl
                                                   model.key
                                                   (modelToUrl
                                                        model.currentSearchTerm
                                                        ( model.currentPage + 1)
                                                   )
                                             , resetViewport
                                             ]
                                     Nothing -> Cmd.none
                               )
                                
                        PageNum num ->
                            ( { model | currentPage = num }
                            , Cmd.batch
                                [ Nav.pushUrl
                                      model.key
                                      (modelToUrl model.currentSearchTerm num)
                                , resetViewport
                                ]
                            )
                NoOp -> (model, Cmd.none)

                ShowCharacterInfo character ->
                    ( { model | currentCharacterOnShow = Just character }
                    , Cmd.none
                    )
                                
        _ -> (model, Cmd.none)

modelToUrl : String -> Int -> String
modelToUrl charName page =
    "search?charname=" ++ charName ++ "&page=" ++ String.fromInt page

--VIEW RESULTS PAGE

viewResultsPage : WindowSize -> (Result model) -> Element Msg
viewResultsPage resultsPageSize model =
    case model.searchResult of
        Failure _ -> text "Something went wrong"
        Loading -> text "Loading"
        NoSearchInitiated -> text "How did you get here?"
        CharacterSearch charRequest ->
            viewCharacterSearchResults
                resultsPageSize
                    model.device
                    model.currentPage
                    charRequest
                    model.currentCharacterOnShow
                    (max 0 (model.scrollPos - 50) )
        _ -> text "This page is not implemented yet"

viewCharacterDetails :Int ->  Maybe Character -> Element Msg
viewCharacterDetails padding currentChar =
    let info =
          case currentChar of
              Nothing -> none
              Just char ->
                  el
                    [ width <| px 200
                    , height <| px 200
                    , Background.color white
                    , alignTop
                    , centerX
                    , Font.color black
                    ]
                    none
    in  el
           -- { options = [ noStaticStyleSheet ] }
            [-- htmlAttribute (Html.Attributes.style "position" "fixed") 
            -- explain Debug.todo
             width fill
            , height fill
            , centerX
            , htmlAttribute <| Html.Attributes.id "character-details-panel"
            , paddingEach { top = padding, bottom = 0, right = 0, left = 0 }
            ]
            info

viewCharacterSearchResults : WindowSize -> Device -> Int -> CharacterRequest
                           -> Maybe Character -> Int -> Element Msg
viewCharacterSearchResults resultsPageSize device pageNum charRequest currentCharacter detailedInfoPadding =
    let paddingCenter =
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
              ] <|
                  let resultSize =
                          { width = resultsColwidth
                          , height = percent 35 resultsColwidth
                          }
                         
                  in case orientation of
                         Landscape ->
                             List.indexedMap
                                 ( viewCharacterResultHorizontal resultSize )
                                 charList
                         Portrait ->
                             List.indexedMap
                                 ( viewCharacterResultVertical resultSize.width )
                                 charList
              
                                                                    
    in column
        [ width fill
        , spacing (percent 5 resultsPageSize.height)
        ]
        [
           case device.orientation of
                    Landscape ->
                        case device.class of
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
                                  [ paddingLeft
                                  , width fill
                                  --, explain Debug.todo
                                  ]
                                  [ resultsColumn
                                        (percent 35 resultsPageSize.width)
                                        Landscape
                                        charRequest.results
                                  , viewCharacterDetails detailedInfoPadding currentCharacter
                                  ]
                    Portrait ->
                        case device.class of
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
        , el
            [ centerX
            ] <|
            viewPageNavigation (percent 4 resultsPageSize.height) pageNum charRequest.info device
        ]
               


{-        
getCurrentPage : Route -> Maybe Int
getCurrentPage route =
    case route of
        SearchResultsPage parameters -> parameters.page
        _ -> Nothing
 -}
                 
viewCharacterResultHorizontal : WindowSize -> Int  -> Character -> Element Msg
viewCharacterResultHorizontal resultSize index character =
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
                index
                character]
    
viewCharacterResultVertical : Width -> Int -> Character -> Element Msg
viewCharacterResultVertical resultWidth index character =
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
         ++ [viewCharacterInfo
                { width = resultWidth
                , height = resultWidth
                }
                index
                character]
         

    
viewCharacterInfo : WindowSize -> Int -> Character -> Element Msg
viewCharacterInfo infoSize index character =
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
              [ Input.button
                    [ Font.bold
                    , Font.size mainFontSize
                    , mouseOver [ Font.color green ]
                    , noFocusShadow
                    , Region.heading 1
                    ]
                    { onPress = Just (ShowCharacterInfo character)
                    , label = text character.name
                    }
              ]
        , subInfo "Status:" (statusToString character.status)
        , subInfo "Species:" ( viewSpecies character.species character.subType )
              
        ]


viewPageNavigation : Height -> Int -> RequestInfo -> Device -> Element Msg
viewPageNavigation navigationHeight currentPageArg info device =
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
            , Border.rounded (percent 30 navigationHeight)
            , height fill
            --, width <| px navigationHeight
            , Font.size <| percent 70 navigationHeight
            , paddingXY (percent 20 navigationHeight) 0
            , Font.color black
            , Font.center
           -- , Font.underline
            , noFocusShadow
            ]
            
        prevButton =
            case info.prev of
                Nothing ->
                    none
                Just _ ->
                    Input.button
                        prevNextAttribute
                        { onPress = Just (GoToResultsPage Prev)
                        , label = text "Prev"
                        }
                        
        nextButton =
            case info.next of
                Nothing ->
                    none
                Just _ ->
                    Input.button
                        prevNextAttribute
                        { onPress = Just (GoToResultsPage Next)
                        , label = text "Next"
                        }
                        
        pageNumberButton : Int -> Element Msg
        pageNumberButton num =
            Input.button
                prevNextAttribute
                { onPress = Just ( GoToResultsPage <| PageNum num )
                , label = text <| String.fromInt num
                }
                
    in case info.pages of
           1 ->
               --dont show navigation if there is only one page
               none
           _ ->
               wrappedRow
                  [ centerX
                  , spacing (percent 50 navigationHeight)
                  , height <| px navigationHeight
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

statusToString : Status -> String
statusToString status =
    case status of
        Alive -> "Alive"
        Dead -> "Dead"
        Unknown -> "unknown"
        InvalidStatus -> "Invalid status"

resetViewport : Cmd Msg
resetViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 0 0)

