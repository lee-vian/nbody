import AnimationFrame
import Array exposing (Array, fromList, map, toList)
import Html exposing (Html)
import Svg exposing (Svg, circle, svg)
import Svg.Attributes as Attr
import Task
import Time exposing (Time, millisecond, second)
import Vector exposing (..)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Body =
  { mass : Float -- Units: m^3 * kg^-1 * s^-2
  , radius : Float -- Units: m
  , position : Vector
  , velocity : Vector
  }

type alias Model =
  { gravitationalConstant : Float
  , x : Float
  , y : Float
  , time : Time
  , bodies : Array Body
  }

init : (Model, Cmd Msg)
init =
  let
    model =
      { gravitationalConstant = 6.674 * (10 ^ -11)
      , x = 100
      , y = 100
      , time = 0
      , bodies = fromList
        [ { mass = 10^13
          , radius = 3 
          , position = zero
          , velocity = zero
          }
        , { mass = 10^13
          , radius = 3
          , position = { x = 100, y = 0 }
          , velocity = zero
          }
        , { mass = 10^14
          , radius = 6
          , position = { x = 50, y = 100 }
          , velocity = zero
          }
        ]
      }
  in
    (model, Task.perform Start Time.now)

-- UPDATE

type Msg
  = Start Time
  | Tick Time

simpleAcceleration : Model -> Body -> Body -> Vector
simpleAcceleration model on by =
  let
    distance = subtract by.position on.position
    normDistance = norm distance
  in
    if normDistance <= on.radius + by.radius then
      zero
    else
      let
        scaleFactor = model.gravitationalConstant * by.mass / (normDistance * normDistance * normDistance)
      in
        scale scaleFactor distance

acceleration : Model -> Body -> Vector
acceleration model on =
  model.bodies |> map (simpleAcceleration model on) |> sum

applyAcceleration : Float -> Vector -> Vector -> Vector
applyAcceleration duration accel vel =
  let
    scaledAccel = scale duration accel
  in
    vel |> add scaledAccel

applyAccelerationAndVelocity : Model -> Float -> Vector -> Vector -> Vector -> Vector
applyAccelerationAndVelocity model duration accel vel pos =
  let
    scaledAccel = scale (duration * duration / 2) accel
    scaledVel = scale duration vel
    newPos = pos |> add scaledVel |> add scaledAccel
  in
    { x = clamp 0 model.x newPos.x
    , y = clamp 0 model.y newPos.y
    }

applyTime : Model -> Time  -> Body -> Body
applyTime model newTime body =
  let
    accel = acceleration model body
    duration = (newTime - model.time) / second
  in
    { body
    | velocity = applyAcceleration duration accel body.velocity
    , position = applyAccelerationAndVelocity model duration accel body.velocity body.position
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newModel = case msg of
      Start newTime ->
        { model
        | time = newTime
        }
      Tick newTime ->
        { model
        | time = newTime
        , bodies = model.bodies |> map (applyTime model newTime)
        }
  in
    (newModel, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times Tick

-- VIEW

dot : Body -> Svg Msg
dot body =
  circle 
    [ Attr.cx <| toString <| body.position.x
    , Attr.cy <| toString <| body.position.y
    , Attr.r  <| toString <| body.radius
    ]
    []

viewBox : Model -> Html.Attribute Msg
viewBox model =
  Attr.viewBox ("0 0 " ++ (toString model.x) ++ " " ++ (toString model.y))

view : Model -> Html Msg
view model =
  svg [ viewBox model, Attr.width "300px" ]
    (model.bodies |> map dot |> toList)