module Spelling exposing (..)

import Browser
import Html exposing (Html, ul, li, text, div, span, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Process
import Task
import Random exposing (..)


-- MAIN


main = Browser.element
  { init = init
  , subscriptions = subscriptions
  , update = update
  , view = view
  }



-- MODEL


type alias Model =
  { wordList : List String
  , confirmation : String
  , currentList : List AnswerChoice
  }


init : () -> (Model, Cmd Msg)
init _ =
  (
    { wordList = ["sun", "mercury", "venus","earth","mars","jupiter","saturn","uranus","neptune","pluto"]
    , confirmation = ""
    , currentList = []
    }
    , Cmd.none
  )

type AnswerChoice
  = Correct String
  | Incorrect String

getWord : AnswerChoice -> String
getWord choice =
  case choice of
    Correct word ->
      word
    Incorrect word ->
      word
getAction : AnswerChoice -> Msg
getAction choice =
    case choice of
      Correct _ ->
        NextWord
      Incorrect _ ->
        SelectIncorrect


createAnswers: Maybe String -> List String -> List AnswerChoice
createAnswers word rest =
  case word of
    Nothing ->
      []
    Just element ->
      [Correct element] ++ (List.map (\a -> Incorrect a) rest)

getRestOrEmpty : List a -> List a
getRestOrEmpty list =
  case (List.tail list) of
    Nothing ->
      []
    Just a ->
      a
-- UPDATE


type Msg
  = SelectIncorrect
  | HideMessage
  | NextWord
  | Shuffle (List Float)
--
--
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectIncorrect ->
      ( { model | confirmation = "Not so great."}
      , Process.sleep 3000 |> Task.perform (always HideMessage)
      )
    HideMessage ->
      ( { model | confirmation = ""}
      , Cmd.none
      )
    NextWord ->
      let
        newChoices = createAnswers (List.head model.wordList) (getRestOrEmpty model.wordList)
      in
      ( {
          model
          | currentList = newChoices
          , wordList = getRestOrEmpty model.wordList
          , confirmation = ""
        }
      , Random.generate Shuffle (randList (List.length newChoices))
      )
    Shuffle rand ->
      ( {
          model
          -- | currentList = List.map (\a -> Tuple.second a) (List.sortWith (\a b -> compare (Tuple.first a) (Tuple.first b)) (List.map2 (\a b -> (a,b)) rand model.currentList))
          | currentList = shuffleList model.currentList rand
        }
      , Cmd.none
      )

randList : Int -> Random.Generator (List Float)
randList count =
    Random.list count (Random.float 0 1)
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

shuffleList : List a -> List comparable -> List a
shuffleList target random =
   List.map
    (\a -> Tuple.second a)
    (List.sortBy
      Tuple.first
      ( List.map2
       (\a b -> (a,b))
       random target
      )
    )
-- VIEW

view : Model -> Html Msg
view model =
  div []
    (
      (
        if List.length model.currentList == 0 && List.length model.wordList > 0 then
          [button [class "btn btn-primary", onClick NextWord] [text "Start"]]
        else
          []
      ) ++
      [ ul []
      (List.map (\choice ->
        li [onClick (getAction choice)] [text (getWord choice)]
      ) model.currentList)
      , span [] [text model.confirmation]
      ]
    )
