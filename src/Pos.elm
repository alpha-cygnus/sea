module Pos exposing (..)

type alias Pos = (Float, Float)

diff (x1, y1) (x0, y0) = (x1 - x0, y1 - y0)
add (x1, y1) (x0, y0) = (x1 + x0, y1 + y0)
mul2 (x1, y1) (x0, y0) = (x1 * x0, y1 * y0)
mul s (x, y) = (s*x, s*y)
