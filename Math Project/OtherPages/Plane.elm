module Plane exposing (main)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (GetKeyState, gameApp)

import Html
import Html.Attributes exposing (..)
import Bootstrap.Button as Button


myShapes model = [rect 200 220 |> filled (rgb 150 210 230) -- Background

                  -- Creates Tower and Plane Images
                  ,image "http://localhost:8000/Math%20Project/Images/Tower.png"|> scale 0.035 |> move (-58,-27)
                  , image "http://localhost:8000/Math%20Project/Images/Tower.png"|> scale 0.035 |> move (40,-27)
                  , imageFit "http://localhost:8000/Math%20Project/Images/Plane.png" 1280 640 |> scale 0.03 |> move ((cos (degrees (toFloat model.angleC)))*model.sideb - 62, (sin (degrees (toFloat model.angleC)))*model.sideb - 22)
                  
                  
                  , rect 200 40 |> filled darkGray |> move (0,-64) -- Ground Shape
                  
                  -- Draws Polygon Connecting the plane to the towers
                  ,openPolygon [(-40,-43),((cos (degrees (toFloat model.angleC)))*model.sideb - 40, (sin (degrees (toFloat model.angleC)))*model.sideb - 43),(40,-43)] |> outlined (dotted 0.5) black 
                  
                  -- Displays angles A, B and C
                  ,text ("C = " ++ (String.fromInt model.angleC) ++ "°") |> alignLeft |> filled black  |> scale 0.5 |> move (-80,-42)
                  ,text ("B = " ++ (String.fromInt model.angleB)  ++ "°") |> alignRight |> filled black  |> scale 0.5 |> move (80,-42)
                  ,text ("A = " ++ (String.fromInt model.angleA)  ++ "°") |> centered |> filled black  |> scale 0.5 |> move ((cos (degrees (toFloat model.angleC)))*model.sideb - 40, (sin (degrees (toFloat model.angleC)))*model.sideb - 42)
                  
                  -- Displays the length of sides a on the diagram
                  , text "a = 80m" |> centered |> filled black  |> scale 0.5 |> move (0,-50)

                  -- Displays the length of sides b and c with the equations
                  ,text ("c = (80/(sin A))*(sin C) = (80/(sin " ++ (String.fromInt model.angleA) ++ "))*(sin " ++ (String.fromInt model.angleC) ++ ") = " ++ (String.fromFloat model.sideb |> String.left 6)  ++ "m") |> alignLeft |> filled black  |> scale 0.3 |> move (-85,-56)
                  ,text ("b = (80/(sin A))*(sin B) = (80/(sin " ++ (String.fromInt model.angleA) ++ "))*(sin " ++ (String.fromInt model.angleB) ++ ") = " ++ (String.fromFloat model.sidec |> String.left 6)  ++ "m") |> alignLeft |> filled black  |> scale 0.3 |> move (-85,-60)
                  
                  -- Creates Back Button Using the buttonLink function
                  , buttonLink "Back" "http://localhost:8000/Math%20Project/OtherPages/Experiment.elm" "black" |>scale 0.75 |> move (65,-50)

                  ] ++ slider model model.pos1 ++ [group (slider model model.pos2) |> move (-170,0) ] -- Displays the sliders

                  -- Displays the length of sides b and c on the diagram
                    ++ (if (model.sideb > 45) then [text ("b = " ++ (String.fromFloat model.sideb |> String.left 5)  ++ "m") |> centered |> filled black  |> scale 0.5 |> move (0,1) |> rotate (degrees (toFloat model.angleC)) |> move ((cos (degrees (toFloat model.angleC)))*(model.sideb/2) - 40, (sin (degrees (toFloat model.angleC)))*(model.sideb/2) - 43)] else [])
                    ++ (if (model.sidec > 45) then [text ("c = " ++ (String.fromFloat model.sidec |> String.left 5)  ++ "m") |> centered |> filled black  |> scale 0.5 |> move (0,1) |> rotate (degrees (toFloat -model.angleB))|> move (40 - (cos (degrees (toFloat model.angleB)))*(model.sidec/2), (sin (degrees (toFloat model.angleB)))*(model.sidec/2) - 42)] else [])
                    


-- Generates the sliders
slider model sliderpos = [rect 5 100 |> filled gray |> move (85,0), 
                          circle 5 |> filled gray |> move sliderpos,
                          circle 5 |> outlined (solid 1) black |> move sliderpos,
                          -- Shows the Slider Text
                          text (Debug.toString (round (((Tuple.second sliderpos) + 43.5)*(86.9/84)))) |> filled black |> scale 0.5 |> move (((Tuple.first sliderpos) - 3), ((Tuple.second sliderpos) - 2)) ] ++
                          -- Detects Slider Movement
                          if (model.pos1 == sliderpos) then [rect 20 85 |> ghost |> move (85,0) |> notifyMouseDownAt ChangeDragStateDown |> notifyMouseUpAt ChangeDragStateUp |> notifyLeave ChangeDragStateLeave |> notifyMouseMoveAt ChangePos] 
                          else if (model.pos2 == sliderpos) then [rect 20 85 |> ghost |> move (85,0) |> notifyMouseDownAt ChangeDragStateDown2 |> notifyMouseUpAt ChangeDragStateUp2 |> notifyLeave ChangeDragStateLeave |> notifyMouseMoveAt ChangePos2] 
                          else []


-- Uses Html to create a linked Html Button
buttonLink inText inLink inTextColor  = ( html 1000 1000 <| Button.button [ Button.primary] [Html.a [target "", href inLink] [Html.h1 [style "text-decoration" "none", style "color" inTextColor] [ Html.text inText]]]) |> scale 0.2
-- Uses Html to create a Text Link 
link inText inLink inTextColor = ( html 1000 100 <| Html.a [target "", href inLink] [Html.h5 [style "text-decoration" "none", style "color" inTextColor] [ Html.text inText]] ) |> scale 0.4 

image inLink = ( html 1000 667 <| Html.img [src inLink] [] ) -- Uses Html to generate a fixed size image
imageFit inLink x y = ( html x y <| Html.img [src inLink] [] ) -- Uses Html to generate a custom resulution image

type DragState = Released | Dragging -- Defines DragState

type Msg = Tick Float GetKeyState | ChangeDragStateDown (Float, Float) | ChangeDragStateUp (Float, Float) | ChangePos (Float, Float) | ChangeDragStateLeave | ChangeDragStateDown2 (Float, Float) | ChangeDragStateUp2 (Float, Float) | ChangePos2 (Float, Float) 


update msg model = case msg of
                    Tick t _ -> { model| time = t
                                -- Updates the side lengths and angles using the slider position
                                , angleC = (round (((Tuple.second model.pos2) + 43.5)*(86.9/84)))
                                , angleB = (round (((Tuple.second model.pos1) + 43.5)*(86.9/84)))
                                , angleA = 180 - model.angleC - model.angleB
                                , sideb = (((sin (degrees (toFloat model.angleB)))*(80))/(sin (degrees (toFloat model.angleA))))
                                , sidec = (((sin (degrees (toFloat model.angleC)))*(80))/(sin (degrees (toFloat model.angleA))))}
                     
                    -- Handles the condition when slider one is clicked down
                    ChangeDragStateDown (_,y) -> { model | dragState = case model.dragState of
                                                                        Released -> Dragging
                                                                        Dragging -> Dragging
                                                                    ,pos1 = (Tuple.first model.pos1, y)
                                                                    }
                    -- Handles the condition when slider one relased
                    ChangeDragStateUp (_,y) -> { model | dragState = case model.dragState of
                                                                        Released -> Released
                                                                        Dragging -> Released
                                                                    ,pos1 = (Tuple.first model.pos1, y)
                                                                    }
                    -- Handles the condition when slider one is moved off of while draging
                    ChangeDragStateLeave -> { model | dragState = case model.dragState of
                                                                        Released -> Released
                                                                        Dragging -> Released
                                                                    }
                    -- Updates slider ones position
                    ChangePos (_,y) -> { model | pos1 = if (model.dragState == Dragging) then (Tuple.first model.pos1, y) else model.pos1} 

                    -- Handles the condition when slider two is clicked down
                    ChangeDragStateDown2 (_,y) -> { model | dragState = case model.dragState of
                                                                        Released -> Dragging
                                                                        Dragging -> Dragging
                                                                    ,pos2 = (Tuple.first model.pos2, y)
                                                                    }
                    -- Handles the condition when slider two relased
                    ChangeDragStateUp2 (_,y) -> { model | dragState = case model.dragState of
                                                                        Released -> Released
                                                                        Dragging -> Released
                                                                    ,pos2 = (Tuple.first model.pos2, y)
                                                                    }
                    -- Updates slider twos position
                    ChangePos2 (_,y) -> { model | pos2 = if (model.dragState == Dragging) then (Tuple.first model.pos2, y) else model.pos2} 


init = { time = 0, pos1 = (85, 10), dragState = Released, pos2 = (85, 10), angleC = 55, angleB = 55, angleA = 70, sideb = (((sin (degrees 55))*(80))/(sin (degrees 70))), sidec = (((sin (degrees 55))*(80))/(sin (degrees 70))) }


main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)