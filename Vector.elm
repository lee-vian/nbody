module Vector exposing (..)

import Array exposing (..)

type alias Vector =
  { x : Float
  , y : Float
  }

zero : Vector
zero =
  { x = 0, y = 0 }

scale : Float -> Vector -> Vector
scale f v =
  { x = f * v.x
  , y = f * v.y
  }

norm : Vector -> Float
norm v =
  sqrt (v.x * v.x + v.y * v.y)

add : Vector -> Vector -> Vector
add v1 v2 =
  { x = v1.x + v2.x
  , y = v1.y + v2.y
  }

subtract : Vector -> Vector -> Vector
subtract v1 v2 =
  { x = v1.x - v2.x
  , y = v1.y - v2.y
  }

sum : Array Vector -> Vector
sum =
  foldl add zero
