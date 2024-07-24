port module Quiz exposing (Quiz, encode, send, update, view)

import Array exposing (Array)
import Array.Extra
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Events
import Json.Decode
import Json.Encode
import List.Extra
import Set


type Quiz
    = Quiz Internals


type alias Internals =
    { values : Array (List String)
    , stack : Stack
    , lookup : Lookup
    }


type alias Lookup =
    Dict ( String, String ) Favorite


type alias Favorite =
    String


type alias Stack =
    { decisions : List Decision
    , low : Int
    , high : Int
    }


type alias Decision =
    { left : String
    , right : String
    , selection : Maybe String
    }


update : String -> Quiz -> Quiz
update choice (Quiz internals) =
    let
        updatedDecisions : List Decision
        updatedDecisions =
            -- assume the next decision is the first one with selection = Nothing
            List.foldl
                (updateDecisions choice)
                { decisions = [], completed = False }
                internals.stack.decisions
                |> .decisions

        stack : Stack
        stack =
            internals.stack

        allDecided : Bool
        allDecided =
            List.all (\decision -> decision.selection /= Nothing) updatedDecisions

        updatedQuiz =
            if allDecided then
                Quiz { internals | stack = { stack | decisions = updatedDecisions } }
                    |> handleAllDecided
                -- quiz with updated values
                -- new decisions calculated

            else
                Quiz { internals | stack = { stack | decisions = updatedDecisions } }
    in
    updatedQuiz


handleAllDecided : Quiz -> Quiz
handleAllDecided (Quiz internals) =
    let
        newLookup : Lookup
        newLookup =
            internals.stack.decisions
                |> fromDecisionsToLookup
                |> Dict.union internals.lookup

        combinedList : List String
        combinedList =
            internals.stack.decisions
                |> decisionsToUniqueStrings
                |> List.sortWith (uniqueStringSorter newLookup)

        newValues : Array (List String)
        newValues =
            internals.values
                |> Array.Extra.removeAt internals.stack.high
                |> Array.Extra.removeAt internals.stack.low
                |> Array.push combinedList

        newStack : Stack
        newStack =
            case Array.toList newValues of
                [] ->
                    internals.stack

                x :: [] ->
                    internals.stack

                left :: right :: _ ->
                    { low = 0
                    , high = 1
                    , decisions =
                        left
                            |> List.concatMap (\l -> List.map (Tuple.pair l) right)
                            |> List.map
                                (\( l, r ) ->
                                    Decision l r Nothing
                                )

                    -- |> List.append internals.stack.decisions
                    }
    in
    Quiz { internals | values = newValues, stack = newStack, lookup = newLookup }


fromDecisionsToLookup : List Decision -> Lookup
fromDecisionsToLookup decisions =
    decisions
        |> List.map
            (\d ->
                let
                    value =
                        if Just d.left == d.selection then
                            d.left

                        else if Just d.right == d.selection then
                            d.right

                        else
                            -- this is wrong
                            d.left
                in
                if d.left < d.right then
                    ( ( d.left, d.right ), value )

                else
                    ( ( d.right, d.left ), value )
            )
        |> Dict.fromList


decisionsToUniqueStrings : List Decision -> List String
decisionsToUniqueStrings decisions =
    decisions
        |> List.concatMap (\{ left, right } -> [ left, right ])
        |> Set.fromList
        |> Set.toList


uniqueStringSorter : Lookup -> String -> String -> Order
uniqueStringSorter lookup a b =
    let
        favorite =
            if a < b then
                Dict.get ( a, b ) lookup

            else
                Dict.get ( b, a ) lookup
    in
    if Just a == favorite then
        LT

    else if Just b == favorite then
        GT

    else
        EQ


updateDecisions : String -> Decision -> { decisions : List Decision, completed : Bool } -> { decisions : List Decision, completed : Bool }
updateDecisions choice current state =
    if state.completed then
        { decisions = state.decisions ++ [ current ]
        , completed = True
        }

    else if current.selection == Nothing then
        { decisions = state.decisions ++ [ { current | selection = Just choice } ]
        , completed = True
        }

    else
        { decisions = state.decisions ++ [ current ]
        , completed = False
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
    case getFirstUndecided quiz.stack.decisions of
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
                (case Array.get 0 quiz.values of
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
    Json.Decode.map3 Internals
        (Json.Decode.field "values" valuesDecoder)
        (Json.Decode.field "stack" stackDecoder)
        (Json.Decode.field "lookup" lookupDecoder)


stackDecoder : Json.Decode.Decoder Stack
stackDecoder =
    Json.Decode.map3 Stack
        (Json.Decode.field "decisions" (Json.Decode.list decisionDecoder))
        (Json.Decode.field "low" Json.Decode.int)
        (Json.Decode.field "high" Json.Decode.int)


lookupDecoder : Json.Decode.Decoder Lookup
lookupDecoder =
    let
        toKeys : Dict String Favorite -> Dict ( String, String ) Favorite
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
    Json.Decode.dict Json.Decode.string
        |> Json.Decode.map toKeys


valuesDecoder : Json.Decode.Decoder (Array (List String))
valuesDecoder =
    Json.Decode.array (Json.Decode.list Json.Decode.string)


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
          , Json.Encode.array (Json.Encode.list Json.Encode.string) internals.values
          )
        , ( "stack"
          , encodeStack internals.stack
          )
        , ( "lookup"
          , encodeLookup internals.lookup
          )
        ]


encodeStack : Stack -> Json.Encode.Value
encodeStack stack =
    Json.Encode.object
        [ ( "low"
          , Json.Encode.int stack.low
          )
        , ( "high"
          , Json.Encode.int stack.high
          )
        , ( "decisions"
          , Json.Encode.list encodeDecision stack.decisions
          )
        ]


encodeLookup : Lookup -> Json.Encode.Value
encodeLookup lookup =
    Json.Encode.dict encodeLookupKey Json.Encode.string lookup


encodeLookupKey : ( String, String ) -> String
encodeLookupKey ( left, right ) =
    left ++ "\n" ++ right


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
