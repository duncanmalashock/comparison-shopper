port module Quiz exposing (Quiz, encode, send, view)

import Html exposing (Html)
import Html.Events
import Json.Decode
import Json.Encode


type Quiz
    = Quiz Internals


type alias Internals =
    { values : List (List String)
    , decisions : List Decision
    }


type alias Decision =
    { left : String
    , right : String
    , selection : Maybe String
    }


view :
    { userClicked : String -> msg
    , quiz : Quiz
    }
    -> Html msg
view props =
    let
        (Quiz quiz) =
            props.quiz
    in
    case getFirstUndecided quiz.decisions of
        Just { left, right } ->
            Html.div []
                [ Html.button
                    [ Html.Events.onClick (props.userClicked left)
                    ]
                    [ Html.text left ]
                , Html.button
                    [ Html.Events.onClick (props.userClicked right)
                    ]
                    [ Html.text right ]
                ]

        Nothing ->
            Html.div []
                (case List.head quiz.values of
                    Nothing ->
                        [ Html.text "what the hell" ]

                    Just head ->
                        List.map Html.text head
                )


getFirstUndecided : List Decision -> Maybe Decision
getFirstUndecided list =
    list
        |> List.filter (\decision -> decision.selection == Nothing)
        |> List.head


decoder : Json.Decode.Decoder Quiz
decoder =
    Json.Decode.map Quiz internalsDecoder


internalsDecoder : Json.Decode.Decoder Internals
internalsDecoder =
    Json.Decode.map2 Internals
        (Json.Decode.field "values" valuesDecoder)
        (Json.Decode.field "decisions" (Json.Decode.list decisionDecoder))


valuesDecoder : Json.Decode.Decoder (List (List String))
valuesDecoder =
    Json.Decode.list (Json.Decode.list Json.Decode.string)


decisionDecoder : Json.Decode.Decoder Decision
decisionDecoder =
    Json.Decode.map3 Decision
        (Json.Decode.field "left" Json.Decode.string)
        (Json.Decode.field "right" Json.Decode.string)
        (Json.Decode.field "selection" (Json.Decode.maybe Json.Decode.string))


encode : Quiz -> Json.Encode.Value
encode (Quiz internals) =
    Json.Encode.object
        [ ( "values"
          , Json.Encode.list (Json.Encode.list Json.Encode.string) internals.values
          )
        , ( "decisions"
          , Json.Encode.list encodeDecision internals.decisions
          )
        ]


encodeDecision : Decision -> Json.Encode.Value
encodeDecision decision =
    Json.Encode.object
        [ ( "left"
          , Json.Encode.string decision.left
          )
        , ( "right"
          , Json.Encode.string decision.right
          )
        , ( "selection"
          , case decision.selection of
                Just selection ->
                    Json.Encode.string selection

                Nothing ->
                    Json.Encode.null
          )
        ]


port sendQuiz : (Json.Encode.Value -> msg) -> Sub msg


send : (Result Json.Decode.Error Quiz -> msg) -> Sub msg
send fromResult =
    sendQuiz
        (\json ->
            json
                |> Json.Decode.decodeValue decoder
                |> fromResult
        )
