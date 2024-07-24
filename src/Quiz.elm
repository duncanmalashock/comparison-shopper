port module Quiz exposing (Quiz, encode, send, update, view)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Events
import Json.Decode
import Json.Encode
import List.Extra


type Quiz
    = Quiz Internals


type alias Internals =
    { options : List String
    , preferences : Preferences
    }


type alias Preferences =
    Dict ( String, String ) Order


unknowns : Quiz -> List ( String, String )
unknowns (Quiz internals) =
    List.Extra.uniquePairs internals.options
        |> List.filter (isUnknown internals.preferences)


isUnknown : Preferences -> ( String, String ) -> Bool
isUnknown preferences pair =
    Dict.get pair preferences
        |> (==) Nothing


sortWithPreferences : Preferences -> List String -> List String
sortWithPreferences preferences options =
    let
        compareWithPreferences : String -> String -> Order
        compareWithPreferences a b =
            case Dict.get ( a, b ) preferences of
                Just order ->
                    order

                Nothing ->
                    case Dict.get ( b, a ) preferences of
                        Just order ->
                            case order of
                                LT ->
                                    GT

                                EQ ->
                                    EQ

                                GT ->
                                    LT

                        Nothing ->
                            Basics.compare a b
    in
    List.sortWith compareWithPreferences options


update : { left : String, right : String, order : Order } -> Quiz -> Quiz
update preference (Quiz internals) =
    Quiz
        { internals
            | preferences =
                Dict.insert
                    ( preference.left, preference.right )
                    preference.order
                    internals.preferences
        }


view :
    { userClicked : { left : String, right : String, order : Order } -> msg
    , quiz : Quiz
    }
    -> Html msg
view props =
    let
        (Quiz internals) =
            props.quiz
    in
    case firstUnknown props.quiz of
        Just ( left, right ) ->
            Html.div []
                [ Html.button
                    [ Html.Events.onClick
                        (props.userClicked
                            { left = left, right = right, order = LT }
                        )
                    ]
                    [ Html.text left ]
                , Html.button
                    [ Html.Events.onClick
                        (props.userClicked
                            { left = left, right = right, order = GT }
                        )
                    ]
                    [ Html.text right ]
                ]

        Nothing ->
            Html.div []
                (sortWithPreferences internals.preferences internals.options
                    |> List.map Html.text
                )


firstUnknown : Quiz -> Maybe ( String, String )
firstUnknown quiz =
    quiz
        |> unknowns
        |> List.head


decoder : Json.Decode.Decoder Quiz
decoder =
    Json.Decode.map Quiz internalsDecoder


internalsDecoder : Json.Decode.Decoder Internals
internalsDecoder =
    Json.Decode.map2 Internals
        (Json.Decode.field "options" optionsDecoder)
        (Json.Decode.field "preferences" preferencesDecoder)


preferencesDecoder : Json.Decode.Decoder Preferences
preferencesDecoder =
    let
        toKeys : Dict String Order -> Dict ( String, String ) Order
        toKeys d =
            Dict.toList d
                |> List.map
                    (\( key, value ) ->
                        ( case String.split "\n" key of
                            [] ->
                                ( "", "" )

                            x :: [] ->
                                ( "", "" )

                            a :: b :: _ ->
                                ( a, b )
                        , value
                        )
                    )
                |> Dict.fromList
    in
    Json.Decode.dict orderDecoder
        |> Json.Decode.map toKeys


orderDecoder : Json.Decode.Decoder Order
orderDecoder =
    Json.Decode.string
        |> Json.Decode.andThen decodeOrder


decodeOrder : String -> Json.Decode.Decoder Order
decodeOrder str =
    case str of
        "LT" ->
            Json.Decode.succeed LT

        "EQ" ->
            Json.Decode.succeed EQ

        "GT" ->
            Json.Decode.succeed GT

        _ ->
            Json.Decode.fail ("Invalid Order: " ++ str)


optionsDecoder : Json.Decode.Decoder (List String)
optionsDecoder =
    Json.Decode.list Json.Decode.string


encode : Quiz -> Json.Encode.Value
encode (Quiz internals) =
    Json.Encode.object
        [ ( "options"
          , Json.Encode.list Json.Encode.string internals.options
          )
        , ( "preferences"
          , encodePreferences internals.preferences
          )
        ]


encodePreferences : Preferences -> Json.Encode.Value
encodePreferences preferences =
    Json.Encode.dict encodePreferencesKey Json.Encode.string (preferencesToStringOrder preferences)


preferencesToStringOrder : Preferences -> Dict ( String, String ) String
preferencesToStringOrder preferences =
    Dict.map orderToString preferences


orderToString : ( String, String ) -> Order -> String
orderToString key order =
    case order of
        LT ->
            "LT"

        GT ->
            "GT"

        EQ ->
            "EQ"


encodePreferencesKey : ( String, String ) -> String
encodePreferencesKey ( left, right ) =
    left ++ "\n" ++ right


port sendQuiz : (Json.Encode.Value -> msg) -> Sub msg


send : (Result Json.Decode.Error Quiz -> msg) -> Sub msg
send fromResult =
    sendQuiz
        (\json ->
            json
                |> Json.Decode.decodeValue decoder
                |> fromResult
        )
