module Pages.Quiz.Id_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html
import Page exposing (Page)
import Quiz exposing (Quiz)
import Route exposing (Route)
import Shared
import View exposing (View)
import Json.Decode

page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init route
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type Model
    = Loading
    | Failure
    | Success Quiz


init : Route { id : String } -> () -> ( Model, Effect Msg )
init route () =
    ( Loading
    , Effect.getQuiz route.params.id
    )



-- UPDATE


type Msg
    = UserClicked String
    | GotQuiz (Result Json.Decode.Error Quiz)  


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UserClicked choice ->
            ( model
            , Effect.none
            )

        GotQuiz (Ok quiz) ->
            ( Success quiz
            , Effect.none
            )

        GotQuiz (Err message) ->
            ( Failure
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Quiz.send GotQuiz



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Quiz.Id_"
    , body =
        [ case model of
            Loading ->
                Html.text "Loading..."

            Failure ->
                Html.text "Failed to load quiz"

            Success quiz ->
                Quiz.view
                    { userClicked = UserClicked
                    , quiz = quiz
                    }
        ]
    }
