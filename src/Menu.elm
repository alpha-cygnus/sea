module Menu exposing (Item, Model, init, view)

import Html exposing (Html, text, div, h1, img, span, ul, li)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick, onMouseDown)
import Pos
import String exposing (fromFloat)

type alias Item msg =
  {
    msg: msg,
    caption: String
  }

type alias Model msg =
  {
    pos: Pos.Pos,
    items: List (Item msg)
  }

init : Pos.Pos -> List (Item msg) -> Model msg
init pos items = {items = items, pos = pos}

view : Model msg -> List (Html msg)
view {pos, items} =
  let
    (x, y) = pos
  in
    [div [style "left" (fromFloat x ++ "px"), style "top" (fromFloat y ++ "px"), class "menu"]
      <| List.map (\{msg, caption} -> div [onClick msg, class "item"] [text caption]) items]
