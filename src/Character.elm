module Character exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline as DP

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
