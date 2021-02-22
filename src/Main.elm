module Main exposing (..)

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
import Html exposing (Html, span, div, text, button, input, map, output, hr, h1, h2, h3)
import Html.Attributes exposing (value, placeholder, type_, class, style)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Katex as K exposing ( Latex, human, inline, display, print)
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

--- type definitions ---
type WorldCoordinates
    = WorldCoordinates

type alias Model =
  {
    azimuth   : Angle
  , elevation : Angle
  , orbiting  : Bool
  , angularMomentum : Int
  , totalAngularMomentum : Float
  , isTotalAngularMomentum : Bool
  }

type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | Reset
    | TotalAngular
    | ChangeL Int

type Direction
  = X | Y | Z

type AngularMomentum
  = L | J


--- main ---
main : Program () Model Msg
main =
  Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

init : () -> ( Model, Cmd Msg )
init () =
    ( {
        azimuth  = Angle.degrees 45
      , elevation = Angle.degrees 30
      , orbiting  = False
      , angularMomentum = 1
      , totalAngularMomentum = 1.5
      , isTotalAngularMomentum = False
      }
    , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ChangeL l ->
          ( { model |   angularMomentum = l
                      , totalAngularMomentum = ((toFloat l) + 0.5)
            }
          , Cmd.none )
        TotalAngular ->
          ( { model | isTotalAngularMomentum = not model.isTotalAngularMomentum },
          Cmd.none )
        Reset ->
            ( { model |   azimuth = Angle.degrees 0
                        , elevation = Angle.degrees 0
              }
              , Cmd.none )
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


view : Model -> Browser.Document Msg
view model =
    let
        -- Create a viewpoint by orbiting around a Z axis through the given
        -- focal point, with azimuth measured from the positive X direction
        -- towards positive Y
        angularMomentum =
          case model.isTotalAngularMomentum of
            True -> (toFloat model.angularMomentum) + 0.5
            False -> (toFloat model.angularMomentum)
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
    { title = "Vector Model for Angular Momenta (Quantum Mechanics)"
    , body =
        [
      Scene3d.unlit
            { camera = camera
            , clipDepth = Length.meters 0.1
            , dimensions = ( Pixels.int 800, Pixels.int 600 )
            , background = Scene3d.transparentBackground
            , entities = List.concat
                          [ arrows angularMomentum
                          , ringU angularMomentum 50 X
                          , ringU angularMomentum 50 Y
                          , ringU angularMomentum 50 Z
                          , coords
                          ]
            }
        , h1 [] [ text "Control" ]
        , div []
          (List.concat [
            [ text "Choose orbital angular momentum " ]
          , [ (K.generate htmlGenerator) <| inline "l" ]
          , [ text ": " ]
         -- , genLButtons (List.range 0 10)
          ])
        , div [] (List.concat [ genLButtons (List.range 0 10) ] )
        , h2 [] [ text "View" ]
        , div []
        [
          text "Show total angular momentum: "
        , input [ type_ "checkbox", onClick TotalAngular ] []
        ]
        , div []
            [ button [ onClick Reset ] [ text "xz-Projection" ]
            ]
        , h1 [] [ text "Results" ]
        ] ++ results model

    }

htmlGenerator isDisplayMode stringLatex =
    case isDisplayMode of
        Just True ->
            div [] [ text stringLatex ]

        _ ->
            span [] [ text stringLatex ]

results model =
  let
        orbitalAnuglar = toFloat model.angularMomentum
        headingL = "Orbital angular momentum"
        charL = "l"
        textL = "orbital angular momentum "
        passL = (headingL, charL, textL)

        totalAngular = model.totalAngularMomentum
        headingJ = "Total angular momentum"
        charJ = "j"
        textJ = "total angular momentum "
        passJ = (headingJ, charJ, textJ)

  in
         showResult passL orbitalAnuglar
      ++ showResult passJ totalAngular

showResult (heading, char, content) angularMomentum =
   let
       length = sqrt ( angularMomentum * (angularMomentum + 1))
       lengthText c = String.concat
         [ "|\\vec{",c,"}| = \\sqrt{",c,"\\cdot(",c,"+ 1)} \\hbar" ]
   in
       [ h2 [] [ text heading ]
        , [ human content
          , inline char
          , human " = "
          , human (String.fromFloat angularMomentum)
          ] |> List.map (K.generate htmlGenerator) |> div []
        , [ inline (lengthText char)
          , human " = "
          , human (String.fromFloat length)
          , inline "\\hbar"
          ] |> List.map (K.generate htmlGenerator) |> div []
        ]

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

---- Helper functions -------

toCartesianU r phi direc =
  let
      x = r * (cos phi)
      y = r * (sin phi)
  in
      case direc of
        X -> Point3d.meters x y 0
        Y -> Point3d.meters 0 x y
        Z -> Point3d.meters x 0 y

ringU l numSeg direc =
  let
      len = sqrt ( l * ( l + 1 ) )
      step = 2 * pi / (toFloat numSeg)
      color = Material.color Color.blue

      values = List.map toFloat <| List.range 0 numSeg
      valuesT = List.map (\x -> (toCartesianU len (x*step) direc
                               , toCartesianU len ((x+1)*step) direc )) values
  in
      List.map (Scene3d.lineSegment color)
        <| List.map (LineSegment3d.fromEndpoints) valuesT

coords =
  let
      len1 = Length.meters (-15)
      len2 = Length.meters 15

      xcoord = LineSegment3d.along Axis3d.x len1 len2
      ycoord = LineSegment3d.along Axis3d.y len1 len2
      zcoord = LineSegment3d.along Axis3d.z len1 len2

      color = Material.color Color.black
  in
      List.map (Scene3d.lineSegment color) [xcoord, ycoord, zcoord]



-- getCoord : Float -> Float -> (Point3d Meters WorldCoordinates)
getCoord l ml =
  let
      len = sqrt ( l * ( l + 1 ) )
      phi = asin ( ml / len )
      y = len * (cos phi)
      z = len * (sin phi)
  in
      Point3d.meters 0 y z

arrowHead l ml len dir color =
    let
       tmp = Cone3d.along dir
              { base   = Length.meters (0.9 * len)
              , tip    = Length.meters len
              , radius = Length.meters 0.05
              }
    in Scene3d.cone color tmp


arrowBody l ml len dir color =
  let
       tmp = Cylinder3d.along dir
              { start  = Length.meters 0.0
              , end    = Length.meters (0.9 * len)
              , radius = Length.meters 0.02
              }
    in Scene3d.cylinder color tmp

arrow l ml len color =
    let
        origin = Point3d.meters 0 0 0
        dir =
         case Axis3d.throughPoints origin (getCoord l ml) of
           Just ret -> ret
           Nothing -> Axis3d.z
    in [ (arrowHead l ml len dir color)
       , (arrowBody l ml len dir color)
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
{-| Use movementX and movementY for simplicity (don't need to store initial
mouse position in the model) - not supported in Internet Explorer though
-}
decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))

-- genLButtons : [Int] -> [Html msg]
genLButtons ls = List.map (genLButton) ls

-- genLButton : Int -> Html msg
genLButton l = button [ onClick (ChangeL l)] [ text (String.fromInt l)]
