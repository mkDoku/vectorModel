module Arrow exposing (..)

{-| This example is very similar to the HelloWorld example but shows how to
render individual triangles.
-}

import Angle exposing (Angle)
import Axis3d
import Browser
import Browser.Events
import Camera3d
import Color
import Cone3d
import Cylinder3d
import Direction3d
import Html exposing (Html, div, text)
import Html.Events.Extra.Touch as Touch
import Json.Decode as Decode exposing (Decoder)
import Length
import LineSegment3d
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import Triangle3d
import TriangularMesh
import Viewpoint3d


main : Program () Model Msg
main =
  Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type WorldCoordinates
    = WorldCoordinates

type alias Model =
  {
    azimuth   : Angle
  , elevation : Angle
  , orbiting  : Bool
  }

type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | StartAt ( Float, Float )
    | MoveAt ( Float, Float )
    | EndAt ( Float, Float )

touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )

toCartesian r phi =
  let
      x = r * (cos phi)
      y = r * (sin phi)
  in Point3d.meters x y 0

toCartesian2 r phi =
  let
      y = r * (cos phi)
      z = r * (sin phi)
  in Point3d.meters 0 y z

toCartesian3 r phi =
  let
      x = r * (cos phi)
      z = r * (sin phi)
  in Point3d.meters x 0 z

ring l numSeg =
  let
      len = sqrt ( l * ( l + 1 ) )
      step = 2 * pi / (toFloat numSeg)
      color = Material.color Color.blue
      values = List.map toFloat <| List.range 0 numSeg
      valuesT = List.map (\x -> (toCartesian len (x*step), toCartesian len ((x+1)*step))) values
  in
      List.map (Scene3d.lineSegment color)
        <| List.map (LineSegment3d.fromEndpoints) valuesT

ring2 l numSeg =
  let
      len = sqrt ( l * ( l + 1 ) )
      step = 2 * pi / (toFloat numSeg)
      color = Material.color Color.blue
      values = List.map toFloat <| List.range 0 numSeg
      valuesT = List.map (\x -> (toCartesian2 len (x*step), toCartesian2 len ((x+1)*step))) values
  in
      List.map (Scene3d.lineSegment color)
        <| List.map (LineSegment3d.fromEndpoints) valuesT

ring3 l numSeg =
  let
      len = sqrt ( l * ( l + 1 ) )
      step = 2 * pi / (toFloat numSeg)
      color = Material.color Color.blue
      values = List.map toFloat <| List.range 0 numSeg
      valuesT = List.map (\x -> (toCartesian3 len (x*step), toCartesian3 len ((x+1)*step))) values
  in
      List.map (Scene3d.lineSegment color)
        <| List.map (LineSegment3d.fromEndpoints) valuesT

coords =
  let
      len1 = Length.meters (-5)
      len2 = Length.meters 5

      xcoord = LineSegment3d.along Axis3d.x len1 len2
      ycoord = LineSegment3d.along Axis3d.y len1 len2
      zcoord = LineSegment3d.along Axis3d.z len1 len2

      color = Material.color Color.blue
  in
      List.map (Scene3d.lineSegment color) [xcoord, ycoord, zcoord]


origin = Point3d.meters 0 0 0

-- getCoord : Float -> Float -> (Point3d Meters WorldCoordinates)
getCoord l ml =
  let
      len = sqrt ( l * ( l + 1 ) )
      phi = asin ( ml / len )
      y = len * (cos phi)
      z = len * (sin phi)
  in
      Point3d.meters 0 y z

testCone l ml len dir color =
    let
       tmp = Cone3d.along dir
              { base   = Length.meters (0.9 * len)
              , tip    = Length.meters len
              , radius = Length.meters 0.05
              }
    in Scene3d.cone color tmp


testCylinder l ml len dir color =
  let
       tmp = Cylinder3d.along dir
              { start  = Length.meters 0.0
              , end    = Length.meters (0.9 * len)
              , radius = Length.meters 0.02
              }
    in Scene3d.cylinder color tmp

arrow l ml len color =
    let dir =
         case Axis3d.throughPoints origin (getCoord l ml) of
           Just ret -> ret
           Nothing -> Axis3d.z
    in [ (testCone     l ml len dir color)
       , (testCylinder l ml len dir color)
       ]


genList l = genListHelp (-l) l []

genListHelp l lmax ls =
  case (l == lmax) of
    True -> [l] ++ ls
    False -> [l] ++ (genListHelp (l+1) lmax ls)

arrows l =
  let
      len = sqrt ( l * ( l + 1 ) )
      color = Material.color Color.blue
      tmp = genList l
  in
      List.concat
        <| List.map (\ml -> arrow l ml len color) tmp

init : () -> ( Model, Cmd Msg )
init () =
    -- ( { mesh = Mesh.indexedTriangles mesh1
    ( {
        azimuth  = Angle.degrees 45
      , elevation = Angle.degrees 30
      , orbiting  = False
      }
    , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        -- Start orbiting when a mouse button is pressed
        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        -- Stop orbiting when a mouse button is released
        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        -- Orbit camera on mouse move (if a mouse button is down)
        MouseMove dx dy ->
            if model.orbiting then
                let
                    -- How fast we want to orbit the camera (orbiting the
                    -- camera by 1 degree per pixel of drag is a decent default
                    -- to start with)
                    rotationRate =
                        Angle.degrees 1 |> Quantity.per Pixels.pixel

                    -- Adjust azimuth based on horizontal mouse motion (one
                    -- degree per pixel)
                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    -- Adjust elevation based on vertical mouse motion (one
                    -- degree per pixel), and clamp to make sure camera cannot
                    -- go past vertical in either direction
                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        StartAt _ -> ( model, Cmd.none )
        MoveAt  _ -> ( model, Cmd.none )
        EndAt   _ -> ( model, Cmd.none )

{-| Use movementX and movementY for simplicity (don't need to store initial
mouse position in the model) - not supported in Internet Explorer though
-}
decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))

gold :  Material.Uniform WorldCoordinates
gold = Material.nonmetal
               { baseColor = Color.rgb255 255 195 86
               , roughness = 0.4
               }


--
view : Model -> Browser.Document Msg
view model =
    let
        -- Create a viewpoint by orbiting around a Z axis through the given
        -- focal point, with azimuth measured from the positive X direction
        -- towards positive Y
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.meters 0 0 0
                , azimuth = model.azimuth -- 45
                , elevation = model.elevation -- 30
                , distance = Length.meters 10
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    { title = "Test Mesh"
    , body =
        [
        div
            [ Touch.onStart (StartAt << touchCoordinates)
            , Touch.onMove (MoveAt << touchCoordinates)
            , Touch.onEnd (EndAt << touchCoordinates)
            ]
        [text "touch here"]
    , Scene3d.unlit
            { camera = camera
            , clipDepth = Length.meters 0.1
            , dimensions = ( Pixels.int 800, Pixels.int 600 )
            , background = Scene3d.transparentBackground
            , entities = List.concat
                          [ arrows 1.5
                          , ring   1.5 100
                          , ring2  1.5 100
                          , ring3  1.5 100
                          , coords
                          ]
            }
        ]
    }

-- leave subscriptions empty for now
subscriptions : Model -> Sub Msg
subscriptions model =
    if model.orbiting then
        -- If we're currently orbiting, listen for mouse moves and mouse button
        -- up events (to stop orbiting); in a real app we'd probably also want
        -- to listen for page visibility changes to stop orbiting if the user
        -- switches to a different tab or something
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            ]

    else
        -- If we're not currently orbiting, just listen for mouse down events
        -- to start orbiting
        Browser.Events.onMouseDown (Decode.succeed MouseDown)
