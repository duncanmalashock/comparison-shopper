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
    Dict ( String, String ) ()


unknowns : Quiz -> List ( String, String )
unknowns (Quiz internals) =
    List.Extra.uniquePairs internals.options
        |> List.filter (isUnknown internals.preferences)


chosenAndInferred : Preferences -> Preferences
chosenAndInferred preferences =
    Dict.union
        preferences
        (inferredPreferences preferences)


isUnknown : Preferences -> ( String, String ) -> Bool
isUnknown preferences ( l, r ) =
    let
        allPrefs =
            chosenAndInferred preferences

        leftFirstLookup =
            Dict.get ( l, r ) allPrefs

        rightFirstLookup =
            Dict.get ( r, l ) allPrefs
    in
    (leftFirstLookup == Nothing)
        && (rightFirstLookup == Nothing)


inferredPreferences : Preferences -> Preferences
inferredPreferences preferences =
    -- if 1 < 2, and 2 < 3, then 1 < 3
    let
        prefsList : List ( ( String, String ), () )
        prefsList =
            preferences
                |> Dict.toList

        inferredForRight : ( ( String, String ), () ) -> List ( ( String, String ), () )
        inferredForRight ( ( la, ra ), _ ) =
            prefsList
                |> List.filterMap
                    (\( ( lb, rb ), _ ) ->
                        if lb == ra then
                            Just ( ( la, rb ), () )

                        else
                            Nothing
                    )
    in
    -- For each preference (la, ra)
    -- look up all preferences (lb == ra, rb) and return (la, rb)
    prefsList
        |> List.concatMap inferredForRight
        |> Dict.fromList
        |> (\inf -> Dict.diff inf preferences)


sortWithPreferences : Preferences -> List String -> List String
sortWithPreferences preferences options =
    let
        compareWithPreferences : String -> String -> Order
        compareWithPreferences a b =
            case Dict.get ( a, b ) preferences of
                Just _ ->
                    LT

                Nothing ->
                    case Dict.get ( b, a ) preferences of
                        Just _ ->
                            GT

                        Nothing ->
                            EQ
    in
    List.sortWith compareWithPreferences options


reverseOrder : Order -> Order
reverseOrder order =
    case order of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


update : { left : String, right : String } -> Quiz -> Quiz
update preference (Quiz internals) =
    Quiz
        { internals
            | preferences =
                Dict.insert
                    ( preference.left, preference.right )
                    ()
                    internals.preferences
        }


view :
    { userClicked : { left : String, right : String } -> msg
    , quiz : Quiz
    }
    -> Html msg
view props =
    let
        (Quiz internals) =
            props.quiz

        viewChoiceButtons : String -> String -> Html msg
        viewChoiceButtons left right =
            Html.div []
                [ Html.button
                    [ Html.Events.onClick
                        (props.userClicked
                            { left = left, right = right }
                        )
                    ]
                    [ Html.text left ]
                , Html.button
                    [ Html.Events.onClick
                        (props.userClicked
                            { left = right, right = left }
                        )
                    ]
                    [ Html.text right ]
                ]

        allPreferences : Preferences
        allPreferences =
            chosenAndInferred internals.preferences

        viewSortedList : Html msg
        viewSortedList =
            Html.div []
                (sortWithPreferences allPreferences internals.options
                    |> List.map Html.text
                )

        viewUnknowns : Html msg
        viewUnknowns =
            Html.div []
                ([ Html.text "Unknowns:"
                 ]
                    ++ List.map viewUnknown
                        (unknowns props.quiz)
                )

        viewUnknown : ( String, String ) -> Html msg
        viewUnknown ( left, right ) =
            Html.div []
                [ Html.text (left ++ " vs " ++ right)
                ]

        viewPreferences : Html msg
        viewPreferences =
            Html.div []
                ([ Html.text "Preferences:"
                 ]
                    ++ List.map viewPreference
                        (Dict.toList internals.preferences)
                )

        viewInferredPreferences : Html msg
        viewInferredPreferences =
            Html.div []
                ([ Html.text "Inferred:"
                 ]
                    ++ List.map viewPreference
                        (Dict.toList (inferredPreferences internals.preferences))
                )

        viewPreference : ( ( String, String ), () ) -> Html msg
        viewPreference ( ( left, right ), _ ) =
            Html.div []
                [ Html.text (left ++ " over " ++ right)
                ]
    in
    case firstUnknown props.quiz of
        Just ( left, right ) ->
            Html.div []
                [ viewChoiceButtons left right
                , viewSortedList
                , viewUnknowns
                , viewPreferences
                , viewInferredPreferences
                ]

        Nothing ->
            Html.div []
                [ viewSortedList
                , viewUnknowns
                , viewPreferences
                , viewInferredPreferences
                ]


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
        toKeys : Dict String () -> Dict ( String, String ) ()
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
                        , ()
                        )
                    )
                |> Dict.fromList
    in
    Json.Decode.dict (Json.Decode.succeed ())
        |> Json.Decode.map toKeys


orderDecoder : Json.Decode.Decoder String
orderDecoder =
    Json.Decode.string


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


orderToString : ( String, String ) -> () -> String
orderToString key order =
    ""


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
