module Counter exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, value, readonly)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = Int


init : Model
init =
  0



-- UPDATE


type Msg
  = Increment
  | Decrement


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1



-- VIEW

view : Model -> Html Msg
view model =
  div [class "input-group"]
    [ div [class "input-group-prepend"] [ button [ class "btn btn-primary", onClick Decrement ] [ text "-" ] ]
    , input [class "form-control", readonly True, value (String.fromInt model) ] []
    , div [class "input-group-append"] [ button [ class "btn btn-primary", onClick Increment ] [ text "+" ] ]
    ]
