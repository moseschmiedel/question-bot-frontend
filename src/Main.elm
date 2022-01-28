port module Main exposing (..)

import Browser
import Colors.Opaque exposing (black, red, white)
import Element
    exposing
        ( Element
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , height
        , none
        , padding
        , px
        , rgb255
        , rgba255
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import FeatherIcons
import Html exposing (Html)
import Json.Decode as D
import List
import String
import Ui exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg



-- MODEL


type User
    = Username String
    | Anonymous


type QuestionId
    = QuestionId Int


type Votes
    = Voted Int
    | Notvoted Int


type alias OpenQuestionRecord =
    { id : QuestionId
    , question : String
    , author : User
    , votes : Votes
    }


type alias AnsweredQuestionRecord =
    { id : QuestionId
    , question : String
    , author : User
    , votes : Votes
    , answer : String
    }


type Question
    = OpenQuestion OpenQuestionRecord
    | AnsweredQuestion AnsweredQuestionRecord


type alias Model =
    { questions : List Question
    }


upvoteQuestion : Question -> Question
upvoteQuestion question =
    case question of
        OpenQuestion q ->
            OpenQuestion <|
                case q.votes of
                    Notvoted vs ->
                        { q | votes = Voted <| vs + 1 }

                    Voted vs ->
                        { q | votes = Voted vs }

        AnsweredQuestion q ->
            AnsweredQuestion <|
                case q.votes of
                    Notvoted vs ->
                        { q | votes = Voted <| vs + 1 }

                    Voted vs ->
                        { q | votes = Voted vs }


unvoteQuestion : Question -> Question
unvoteQuestion question =
    case question of
        OpenQuestion q ->
            OpenQuestion <|
                case q.votes of
                    Notvoted vs ->
                        { q | votes = Voted vs }

                    Voted vs ->
                        { q | votes = Notvoted <| vs - 1 }

        AnsweredQuestion q ->
            AnsweredQuestion <|
                case q.votes of
                    Notvoted vs ->
                        { q | votes = Voted vs }

                    Voted vs ->
                        { q | votes = Notvoted <| vs - 1 }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { questions =
            [ OpenQuestion (OpenQuestionRecord (QuestionId 1) "Wie?" Anonymous (Notvoted 0)) ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Upvoted QuestionId
    | Unvoted QuestionId
    | QuestionAdded OpenQuestionRecord
    | Send
    | Recv String



-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--


fOnSingleQuestion : QuestionId -> (Question -> Question) -> Question -> Question
fOnSingleQuestion questionId f question =
    case question of
        OpenQuestion q ->
            if questionId == q.id then
                f question

            else
                question

        AnsweredQuestion q ->
            if questionId == q.id then
                f question

            else
                question


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Upvoted questionId ->
            ( { model | questions = List.map (fOnSingleQuestion questionId upvoteQuestion) model.questions }
            , Cmd.none
            )

        Unvoted questionId ->
            ( { model | questions = List.map (fOnSingleQuestion questionId unvoteQuestion) model.questions }
            , Cmd.none
            )

        QuestionAdded questionRecord ->
            ( { model | questions = OpenQuestion questionRecord :: model.questions }
            , Cmd.none
            )

        Send ->
            ( model
            , sendMessage "hi"
            )

        Recv message ->
            ( model
            , Cmd.none
            )



-- SUBSCRIPTIONS
-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv



-- VIEW


viewUser : User -> String
viewUser user =
    case user of
        Anonymous ->
            "Anonymous"

        Username username ->
            username


viewUpvote : QuestionId -> Votes -> Element Msg
viewUpvote questionId votes =
    column [ Font.color red, spacing 3 ] <|
        case votes of
            Voted vs ->
                [ el [ centerX ] <| text <| String.fromInt vs
                , button []
                    { onPress = Just <| Unvoted questionId
                    , label = el [] <| Element.html (FeatherIcons.heart |> FeatherIcons.withSize 32 |> FeatherIcons.withClass "icon-fill-red" |> FeatherIcons.toHtml [])
                    }
                ]

            Notvoted vs ->
                [ el [ centerX ] <| text <| String.fromInt vs
                , button []
                    { onPress = Just <| Upvoted questionId
                    , label = el [] <| Element.html (FeatherIcons.heart |> FeatherIcons.withSize 32 |> FeatherIcons.toHtml [])
                    }
                ]


viewQuestionContainer : List (Element Msg) -> Element Msg
viewQuestionContainer questionContent =
    row
        [ width (900 |> px)
        , spacing 10
        , Bg.color <| rgba255 100 200 255 0.9
        , padding 10
        , Border.rounded 5
        , Border.color <| rgb255 100 180 255
        , Border.width 2
        , Font.size 28
        ]
        questionContent


viewQuestion : Question -> Element Msg
viewQuestion question =
    case question of
        OpenQuestion q ->
            viewQuestionContainer
                [ column [ spacing 20 ]
                    [ el [ Font.italic ] <| text <| viewUser q.author
                    , text q.question
                    ]
                , el [ alignRight ] <| viewUpvote q.id q.votes
                ]

        AnsweredQuestion q ->
            viewQuestionContainer
                [ column [ spacing 20 ]
                    [ el [ Font.italic ] <| text <| viewUser q.author
                    , text q.question
                    ]
                , el [ alignRight ] <| viewUpvote q.id q.votes
                ]


increaseQuestionId : QuestionId -> QuestionId
increaseQuestionId (QuestionId id) =
    QuestionId <| id + 1


newQuestion : QuestionId -> OpenQuestionRecord
newQuestion lastQuestionId =
    OpenQuestionRecord (increaseQuestionId lastQuestionId) "Nochmal wie?" Anonymous (Notvoted 0)


view : Model -> Html Msg
view model =
    Element.layout [] <|
        column [ centerX, spacing 8 ]
            [ apptitle [ centerX ] "Question Bot"
            , el [ height (20 |> px) ] none
            , button [ alignRight ]
                { onPress = Just <| QuestionAdded <| newQuestion <| QuestionId (List.length model.questions)
                , label =
                    row []
                        [ Element.html <| (FeatherIcons.plusSquare |> FeatherIcons.withSize 36 |> FeatherIcons.toHtml [])
                        , text "Frage stellen"
                        ]
                }
            , column [ spacing 10 ]
                (List.map viewQuestion model.questions)
            ]
