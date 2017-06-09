import Array exposing (Array, fromList, map, toList)
import Html exposing (Html)
import Svg exposing (Svg, circle, svg)
import Svg.Attributes as Attr
import Task
import Time exposing (Time, millisecond, second)
import Vector exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

gravitationalConstant : Float
gravitationalConstant = 6.674 * (10 ^ -11)

type alias Body =
  { mass : Float
  , position : Vector
  , velocity : Vector
  }

type alias Model =
  { time : Time
  , bodies : Array Body
  }

init : (Model, Cmd Msg)
init =
  let
    model =
      { time = 0
      , bodies = fromList
        [ { mass = 1, position = zero, velocity = zero}
        , { mass = 1, position = { x = 100, y = 100 }, velocity = zero }
        ]
      }
  in
    (model, Task.perform Start Time.now)

-- UPDATE

type Msg
  = Start Time
  | Tick Time

simpleAcceleration : Body -> Body -> Vector
simpleAcceleration on by =
  if on == by then
    zero
  else
    let
      distance = subtract on.position by.position
      normDistance = norm distance
      scaleFactor = gravitationalConstant * by.mass / (normDistance * normDistance * normDistance)
    in
      scale scaleFactor distance

acceleration : Array Body -> Body -> Vector
acceleration bodies on =
  bodies |> map (simpleAcceleration on) |> sum

applyAcceleration : Float -> Vector -> Vector -> Vector
applyAcceleration duration accel vel =
  let
    scaledAccel = scale duration accel
  in
    vel |> add scaledAccel

applyAccelerationAndVelocity : Float -> Vector -> Vector -> Vector -> Vector
applyAccelerationAndVelocity duration accel vel pos =
  let
    scaledAccel = scale (duration * duration / 2) accel
    scaledVel = scale duration vel
  in
    pos |> add scaledVel |> add scaledAccel

applyTime : Float -> Array Body -> Body -> Body
applyTime duration bodies body =
  let
    accel = acceleration bodies body
  in
    { body
    | velocity = applyAcceleration duration accel body.velocity
    , position = applyAccelerationAndVelocity duration accel body.velocity body.position
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
        let
          secondsElapsed = (newTime - model.time) / second
          newBodies = model.bodies |> map (applyTime secondsElapsed model.bodies)
        in
          { time = newTime
          , bodies = newBodies
          }
  in
    (newModel, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (100 * millisecond) Tick

-- VIEW

radius : Body -> Float
radius body = body.mass ^ (1/3)

dot : Body -> Svg Msg
dot body =
  circle 
    [ Attr.cx <| toString <| body.position.x
    , Attr.cy <| toString <| body.position.y
    , Attr.r  <| toString <| radius body
    ]
    []

view : Model -> Html Msg
view model =
  svg [ Attr.viewBox "0 0 100 100", Attr.width "300px" ]
    (model.bodies |> map dot |> toList)