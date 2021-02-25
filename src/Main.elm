module Main exposing (..)

import Angle exposing (Angle)
import Axis2d
import Axis3d
import Browser
import Browser.Events
import Camera3d
import Color
import Cone3d
import Cylinder3d
import Direction2d
import Direction3d
import Element
import Element.Border
import Element.Input as Input
import Frame2d
import Geometry.Svg as Svg
import Html exposing (Html, button, div, h1, h2, h3, hr, input, map, output, span, text)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Katex as K exposing (Latex, display, human, inline, print)
import Length
import LineSegment3d
import Pixels exposing (Pixels)
import Point2d
import Point3d
import Point3d.Projection
import Quantity exposing (Quantity)
import Rectangle2d
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import Svg exposing (Svg)
import Svg.Attributes
import Triangle3d
import TriangularMesh
import Vector3d
import Viewpoint3d



--- type definitions ---


type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { azimuth : Angle
    , elevation : Angle
    , orbiting : Bool
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
    = X
    | Y
    | Z


type AngularMomentum
    = L
    | J



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
    ( { azimuth = Angle.degrees 0 --  45
      , elevation = Angle.degrees 0 --30
      , orbiting = False
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
            ( { model
                | angularMomentum = l
                , totalAngularMomentum = toFloat l + 0.5
              }
            , Cmd.none
            )

        TotalAngular ->
            ( { model | isTotalAngularMomentum = not model.isTotalAngularMomentum }
            , Cmd.none
            )

        Reset ->
            ( { model
                | azimuth = Angle.degrees 0
                , elevation = Angle.degrees 0
              }
            , Cmd.none
            )

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
            if model.isTotalAngularMomentum then
                toFloat model.angularMomentum + 0.5

            else
                toFloat model.angularMomentum

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

        mlValues =
            genList angularMomentum

        mlCoords =
            mlValues
                |> List.map (getCoord angularMomentum)
                |> List.map (Point3d.projectOntoAxis Axis3d.z)

        vertices =
            --             [ Point3d.meters 0 0 -(sqrt 2) ]
            mlCoords
                -- |> List.map (Point3d.rotateAround Axis3d.z model.azimuth)
                -- |> List.map (Point3d.rotateAround Axis3d.y model.elevation)
                |> List.map (Point3d.Projection.toScreenSpace camera screenRectangle)

        sceneElement =
            Scene3d.unlit
                { camera = camera
                , clipDepth = Length.meters 0.1
                , dimensions = ( Pixels.int 800, Pixels.int 600 )
                , background = Scene3d.transparentBackground
                , entities =
                    List.concat
                        [ arrows angularMomentum
                        , ringU angularMomentum 50 X
                        , ringU angularMomentum 50 Y
                        , ringU angularMomentum 50 Z
                        , coords
                        , dashedLines angularMomentum 50

                        -- dashes
                        -- ztics
                        ]
                }

        combinedList =
            List.map2 Tuple.pair vertices mlValues

        svgLabels =
            combinedList
                |> List.map
                    (\( vertex, ml ) ->
                        Svg.text_
                            [ Svg.Attributes.fill "rgb(92, 92, 92)"
                            , Svg.Attributes.fontFamily "monospace"
                            , Svg.Attributes.fontSize "20px"
                            , Svg.Attributes.stroke "none"
                            , Svg.Attributes.x
                                (String.fromFloat
                                    (Pixels.toFloat
                                        (Point2d.xCoordinate vertex)
                                        - 60
                                    )
                                )
                            , Svg.Attributes.y
                                (String.fromFloat
                                    (Pixels.toFloat
                                        (Point2d.yCoordinate vertex)
                                    )
                                )
                            ]
                            [ Svg.text (stupidStrConvert (String.fromFloat ml) ml model) ]
                            |> Svg.mirrorAcross (Axis2d.through vertex Direction2d.x)
                    )

        svgElement =
            Svg.svg
                [ Svg.Attributes.width <| String.fromInt myWidth
                , Svg.Attributes.height <| String.fromInt myHeight
                ]
                [ Svg.relativeTo topLeftFrame
                    (Svg.g [] svgLabels)
                ]

        screenRectangle =
            Rectangle2d.from Point2d.origin (Point2d.pixels myHeight myWidth)

        allElements =
            Element.layout [] <|
                Element.column [ Element.spacing 12 ] <|
                    [ -- Use Element.inFront to place the SVG element in front of the
                      --WebGL element
                      Element.el
                        [ Element.inFront (Element.html svgElement) ]
                        (Element.html <| sceneElement)
                    , Element.el [] (Element.html (h1 [] [ text "Control" ]))
                    , Element.el []
                        (Element.html
                            (div
                                []
                                (List.concat
                                    [ [ text "Choose orbital angular momentum " ]
                                    , [ K.generate htmlGenerator <| inline "l" ]
                                    , [ text ": " ]
                                    ]
                                )
                            )
                        )
                    , Element.el []
                        (Element.html
                            (div []
                                (List.concat
                                    [ genLButtons
                                        (List.range 0 9)
                                    ]
                                )
                            )
                        )
                    , Element.el [] (Element.html (h2 [] [ text "View" ]))
                    , Element.el [] (Element.html (div [] [ text "Show total angular momentum: ", input [ type_ "checkbox", onClick TotalAngular ] [] ]))
                    , Element.el []
                        (Element.html
                            (div []
                                [ button [ onClick Reset ]
                                    [ text "xz-Projection"
                                    ]
                                ]
                            )
                        )
                    , Element.el [] (Element.html (h1 [] [ text "Results" ]))
                    ]
                        ++ (results model |> List.map Element.html)
    in
    { title = "Vector Model for Angular Momenta (Quantum Mechanics)"
    , body =
        [ allElements
        ]
    }


stupidStrConvert : String -> Float -> Model -> String
stupidStrConvert str ml model =
    if model.isTotalAngularMomentum then
        if ml < 0 then
            str

        else
            String.append "\u{2000}" str

    else if ml < 0 then
        str ++ ".0"

    else
        String.append (String.append "\u{2000}" str) ".0"


htmlGenerator isDisplayMode stringLatex =
    case isDisplayMode of
        Just True ->
            div [] [ text stringLatex ]

        _ ->
            span [] [ text stringLatex ]


results model =
    let
        orbitalAnuglar =
            toFloat model.angularMomentum

        headingL =
            "Orbital angular momentum"

        charL =
            "l"

        textL =
            "orbital angular momentum "

        passL =
            ( headingL, charL, textL )

        totalAngular =
            model.totalAngularMomentum

        headingJ =
            "Total angular momentum"

        charJ =
            "j"

        textJ =
            "total angular momentum "

        passJ =
            ( headingJ, charJ, textJ )
    in
    showResult passL orbitalAnuglar
        ++ showResult passJ totalAngular


showResult ( heading, char, content ) angularMomentum =
    let
        length =
            sqrt (angularMomentum * (angularMomentum + 1))

        lengthText c =
            String.concat
                [ "|\\vec{", c, "}| = \\sqrt{", c, "\\cdot(", c, "+ 1)} \\hbar" ]
    in
    [ h2 [] [ text heading ]
    , [ human content
      , inline char
      , human " = "
      , human (String.fromFloat angularMomentum)
      ]
        |> List.map (K.generate htmlGenerator)
        |> div []
    , [ inline (lengthText char)
      , human " = "
      , human (String.fromFloat length)
      , inline "\\hbar"
      ]
        |> List.map (K.generate htmlGenerator)
        |> div []
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


myHeight =
    800


myWidth =
    600


topLeftFrame =
    Frame2d.atPoint (Point2d.xy Quantity.zero (Pixels.float 600))
        -- Frame2d.atPoint (Point2d.xy Quantity.zero (Pixels.float 600))
        |> Frame2d.reverseY


toCartesianU r phi direc =
    let
        x =
            r * cos phi

        y =
            r * sin phi
    in
    case direc of
        X ->
            Point3d.meters x y 0

        Y ->
            Point3d.meters 0 x y

        Z ->
            Point3d.meters x 0 y


ringU l numSeg direc =
    let
        len =
            sqrt (l * (l + 1))

        step =
            2 * pi / toFloat numSeg

        color =
            Material.color Color.blue

        values =
            List.map toFloat <| List.range 0 numSeg

        valuesT =
            List.map
                (\x ->
                    ( toCartesianU len (x * step) direc
                    , toCartesianU len ((x + 1) * step) direc
                    )
                )
                values
    in
    List.map (Scene3d.lineSegment color) <|
        List.map LineSegment3d.fromEndpoints valuesT


coords =
    let
        len1 =
            Length.meters -50

        len2 =
            Length.meters 50

        xcoord =
            LineSegment3d.along Axis3d.x len1 len2

        ycoord =
            LineSegment3d.along Axis3d.y len1 len2

        zcoord =
            LineSegment3d.along Axis3d.z len1 len2

        color =
            Material.color Color.black
    in
    List.map (Scene3d.lineSegment color) [ xcoord, ycoord, zcoord ]


dashedLines l numSeg =
    let
        mls =
            genList l
    in
    List.map (\ml -> dashedLine l ml numSeg) mls
        |> List.concat


dashedLine l ml numSeg =
    let
        vecToPoint =
            Vector3d.toMeters >> Point3d.fromMeters

        pointToVec =
            Vector3d.fromMeters << Point3d.toMeters

        coord =
            getCoord l ml

        zProjection =
            Point3d.projectOntoAxis Axis3d.z coord

        color =
            Material.color Color.black

        vecCoord =
            pointToVec coord

        zCoord =
            pointToVec zProjection

        diff =
            Vector3d.minus zCoord vecCoord

        numSegB =
            numSeg - 1

        values =
            -- [0,1,2,3,4,5,6,7,8,9]
            -- 2,3,6,7,10,11 filtered
            -- => [0,1,4,5,8,9]
            -- List.map toFloat <| List.range 0 numSegB
            List.map toFloat <| ownList 0 2 2 13

        valuesT =
            List.map
                (\x ->
                    ( vecToPoint
                        (Vector3d.plus zCoord
                            (Vector3d.scaleBy
                                (x
                                    / toFloat
                                        numSeg
                                )
                                diff
                            )
                        )
                    , vecToPoint
                        (Vector3d.plus zCoord
                            (Vector3d.scaleBy
                                ((x + 1)
                                    / toFloat
                                        numSeg
                                )
                                diff
                            )
                        )
                    )
                )
                values
    in
    List.map (Scene3d.lineSegment color) <|
        List.map LineSegment3d.fromEndpoints valuesT



-- Better start
-- [(0, 0)] -> [(0, 0), (2,3)] -> ...


ownList start onLen offLen howM =
    if howM == 0 then
        []

    else
        List.range start (start + onLen - 1)
            ++ ownList (start + onLen + offLen)
                onLen
                offLen
                (howM - 1)


getCoord l ml =
    let
        len =
            sqrt (l * (l + 1))

        phi =
            asin (ml / len)

        y =
            len * cos phi

        z =
            len * sin phi
    in
    Point3d.meters 0 y z


arrowHead l ml len dir color =
    let
        tmp =
            Cone3d.along dir
                { base = Length.meters (0.9 * len)
                , tip = Length.meters len
                , radius = Length.meters 0.05
                }
    in
    Scene3d.cone color tmp


arrowBody l ml len dir color =
    let
        tmp =
            Cylinder3d.along dir
                { start = Length.meters 0.0
                , end = Length.meters (0.9 * len)
                , radius = Length.meters 0.02
                }
    in
    Scene3d.cylinder color tmp


arrow l ml len color =
    let
        origin =
            Point3d.meters 0 0 0

        dir =
            case Axis3d.throughPoints origin (getCoord l ml) of
                Just ret ->
                    ret

                Nothing ->
                    Axis3d.z
    in
    [ arrowHead l ml len dir color
    , arrowBody l ml len dir color
    ]


genList l =
    genListHelp -l l []


genListHelp l lmax ls =
    if l == lmax then
        [ l ] ++ ls

    else
        [ l ] ++ genListHelp (l + 1) lmax ls


arrows l =
    let
        len =
            sqrt (l * (l + 1))

        color =
            Material.color Color.blue

        tmp =
            genList l
    in
    List.concat <|
        List.map (\ml -> arrow l ml len color) tmp


{-| Use movementX and movementY for simplicity (don't need to store initial
mouse position in the model) - not supported in Internet Explorer though
-}
decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))



-- genLButtons : [Int] -> [Html msg]


genLButtons ls =
    List.map genLButton ls



-- genLButton : Int -> Html msg


genLButton l =
    button [ onClick (ChangeL l) ] [ text (String.fromInt l) ]
