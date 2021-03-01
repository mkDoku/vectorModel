port module Main exposing (..)

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
import Element
import Frame2d
import Geometry.Svg as Svg
import Html exposing (button, div, h1, h2, input, span, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (on, onClick)
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as Decode exposing (Decoder, Value)
import Katex as K exposing (human, inline)
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
import Svg exposing (Svg)
import Svg.Attributes exposing (azimuth)
import Vector3d
import Viewpoint3d


{-| Taken from this:
<https://discourse.elm-lang.org/t/elm-scroll-an-effect-manager-for-scroll-events/1032>
For the use see: <https://ellie-app.com/qqp6pVJRQa1/0>
-}
port onWheel : (Value -> msg) -> Sub msg



-- Type Definitions


type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { azimuth : Angle
    , elevation : Angle
    , orbiting : Bool
    , angularMomentum : Int
    , totalAngularMomentum : Float
    , isTotalAngularMomentum : Bool
    , distance : Length.Length
    }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | Projection
    | Reset
    | TotalAngular
    | ChangeL Int
    | Scrolling Float
    | Zoom Zooming
      -- | ScrollIt (Quantity Float Pixels)
    | NoOp


type Direction
    = X
    | Y
    | Z


type Zooming
    = In
    | Out



-- Main


{-| -}
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
    ( { azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , orbiting = False
      , angularMomentum = 1
      , totalAngularMomentum = 1.5
      , isTotalAngularMomentum = False
      , distance = Length.meters 10
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Zoom dir ->
            case dir of
                -- TODO: Don't go smaller than in the Scrolling Event
                In ->
                    ( { model
                        | distance =
                            Length.meters
                                (Length.inMeters model.distance
                                    - 3
                                )
                      }
                    , Cmd.none
                    )

                Out ->
                    ( { model
                        | distance =
                            Length.meters
                                (Length.inMeters model.distance
                                    + 3
                                )
                      }
                    , Cmd.none
                    )

        Reset ->
            ( { model | azimuth = Angle.degrees 45, elevation = Angle.degrees 30 }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        Scrolling flt ->
            if Length.inMeters model.distance + (flt * zoomStep) > 2 then
                -- Check if camera is to close to origin
                -- value of 2 is arbitrary
                ( updateDistanceBy flt model, Cmd.none )

            else
                -- Don't zoom, when to close to origin
                ( model, Cmd.none )

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

        Projection ->
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
                ( updateViewBy dx dy model, Cmd.none )

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

                -- set distance as model variable to be able to change it with the
                -- mousewheel
                , distance = model.distance
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
                , dimensions = ( Pixels.int myHeight, Pixels.int myWidth )
                , background = Scene3d.transparentBackground
                , entities =
                    List.concat
                        [ arrows angularMomentum
                        , ringU angularMomentum 50 X
                        , ringU angularMomentum 50 Y
                        , ringU angularMomentum 50 Z
                        , coords
                        , dashedLines angularMomentum 50
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

        mkHtmlElement x =
            Element.el [] (Element.html x)

        mkHtmlElementDiv x =
            mkHtmlElement (div [] x)

        scenePlusSvg =
            Element.el
                [ Element.inFront (Element.html svgElement) ]
                (Element.html <| sceneElement)

        allElements =
            Element.layout [] <|
                Element.column [ Element.spacing 12 ] <|
                    [ scenePlusSvg
                    , mkHtmlElement (h1 [] [ text "Control" ])
                    ]
                        ++ List.map mkHtmlElementDiv
                            [ [ text "Choose orbital angular momentum "
                              , K.generate htmlGenerator <| inline "l"
                              , text ": "
                              ]
                            , List.concat
                                [ genLButtons
                                    (List.range 0 9)
                                ]
                            , [ text "Show "
                              , K.generate htmlGenerator <| inline "\\vec{j}"
                              , text " instead of "
                              , K.generate htmlGenerator <| inline "\\vec{l}"
                              , text ": "
                              , input [ type_ "checkbox", onClick TotalAngular ] []
                              ]
                            ]
                        ++ [ mkHtmlElement (h2 [] [ text "View" ])
                           , mkHtmlElementDiv
                                [ button [ onClick Projection ]
                                    [ text "xz-Projection"
                                    ]
                                , button [ onClick Reset ]
                                    [ text "Reset"
                                    ]
                                , text "Zoom: "
                                , button [ onClick (Zoom In) ] [ text "+" ]
                                , button [ onClick (Zoom Out) ] [ text "-" ]
                                ]
                           , mkHtmlElement (h1 [] [ text "Results" ])
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


showMls values =
    let
        valuesStr =
            List.map String.fromFloat values
    in
    "[" ++ (String.concat << List.intersperse ",") valuesStr ++ "]"


showResult ( heading, char, content ) angularMomentum =
    let
        length =
            sqrt (angularMomentum * (angularMomentum + 1))

        lengthText c =
            String.concat
                [ "|\\vec{", c, "}| = \\sqrt{", c, "\\cdot(", c, "+ 1)} \\hbar" ]

        mlList =
            genList angularMomentum
    in
    [ h2 [] [ text heading ]
    , [ -- human content
        inline char
      , human " = "
      , human (String.fromFloat angularMomentum)
      , human ", "
      , inline <| "m_{" ++ char ++ "}"
      , human " = "
      , human <| showMls mlList
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
        Sub.batch
            [ Browser.Events.onMouseDown (Decode.succeed MouseDown)

            -- , on "scroll" decodeScroll
            , onWheel
                (\val ->
                    case Decode.decodeValue wheelDecoder val of
                        Ok it ->
                            Scrolling it

                        Err _ ->
                            NoOp
                )

            --      , Wheel.onWheel
            --          (\event ->
            --              Scrolling event.deltaY
            --          )
            ]



-- Helper Functions


{-|


### For `update`

-}
updateDistanceBy : Float -> Model -> Model
updateDistanceBy flt model =
    { model
        | distance =
            Length.meters
                (Length.inMeters model.distance
                    + flt
                    * zoomStep
                )
    }


updateViewBy dx dy model =
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
    { model | azimuth = newAzimuth, elevation = newElevation }


myHeight : number
myHeight =
    600


myWidth : number
myWidth =
    600


zoomStep : Float
zoomStep =
    0.02


topLeftFrame =
    Frame2d.atPoint (Point2d.xy Quantity.zero (Pixels.float myWidth))
        |> Frame2d.reverseY


toCartesianU : Float -> Float -> Direction -> Point3d.Point3d Length.Meters coordinates
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


wheelDecoder : Decoder Float
wheelDecoder =
    -- Decode.map Scrolling
    Decode.field "deltaY" Decode.float



--decodeScroll : Decoder Msg
--decodeScroll =
--    Decode.map ScrollIt
--        (Decode.field "deltaY" (Decode.map Pixels.float Decode.float))


genLButtons ls =
    List.map genLButton ls



-- genLButton : Int -> Html msg


genLButton l =
    button [ onClick (ChangeL l) ] [ text (String.fromInt l) ]
