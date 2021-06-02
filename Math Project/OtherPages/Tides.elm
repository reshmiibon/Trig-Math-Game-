module Tides exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (GetKeyState, gameApp)

import Html
import Html.Attributes exposing (..)
import Bootstrap.Button as Button

myShapes model = [rect 200 220 |> filled (rgb 156 215 255) |> move (-3,40)
                 , beach
                 , water |> move(0,-2)
                 , wave ((((Tuple.second model.pos) + 0.05)*(10/55) * 3 ) + 20) |> move (0,-2)
                 , text "Use the slider to change the amplitude" |> filled black |> scale 0.4 |> move (-90,57)
                 , text "Amplitude (A)  = " |> filled black |> scale 0.4 |> move (-85,50)
                 , text (Debug.toString (round (((Tuple.second model.pos) + 0.05)*(10/55)))) |> filled black |> scale 0.4 |> move (-49,50)
                 , text "Wave Equation:" |> filled black |> scale 0.8 |> move (-20,20)
                 , text "Maximum height:" |> filled black |> scale 0.4 |> move (40,57)
                 , text (Debug.toString (round (((Tuple.second model.pos) + 0.05)*(10/55)) + 10)) |> filled black |> scale 0.4 |> move (75,57)
                 , text "m" |> filled black |> scale 0.4 |> move (80,57)
                 , equation (Debug.toString (round (((Tuple.second model.pos) + 0.05)*(10/55))))
                 , buttonLink "Back" "http://localhost:8000/Math%20Project/OtherPages/Experiment.elm" "black" |>scale 0.75 |> move (80,-50)

                 ] 
                 ++ slider model model.pos

buttonLink inText inLink inTextColor  = ( html 1000 1000 <| Button.button [ Button.primary] [Html.a [target "", href inLink] [Html.h1 [style "text-decoration" "none", style "color" inTextColor] [ Html.text inText]]]) |> scale 0.2


slider model sliderpos = [rect 3 50 |> filled gray |> move (-90,30), 
                          circle 5 |> filled gray |> move sliderpos,
                          circle 5 |> outlined (solid 1) black |> move sliderpos,
                          text (Debug.toString (round (((Tuple.second sliderpos) + 0.05)*(10/55)))) |> filled black |> scale 0.5 |> move (((Tuple.first sliderpos) - 3), ((Tuple.second sliderpos) - 2)) ] ++
                          if (model.pos == sliderpos) then [rect 20 50 |> ghost |> move (-90,30) |> notifyMouseDownAt ChangeDragStateDown |> notifyMouseUpAt ChangeDragStateUp |> notifyLeave ChangeDragStateLeave |> notifyMouseMoveAt ChangePos] else []


beach = group [rect 40 50 |> filled yellow |> move (-85,-40)
              , openPolygon [(-0.5,-3),(1,-10)] |> outlined (solid 0.8) black |> move (-90,-5)
              , curve (-5,-10) [Pull (-1.5,-1) (5,-8)] |> filled (rgb 252 96 96) |> move (-90,0)
              , openPolygon [(-0.5,-5),(1,-10)] |> outlined (solid 0.8) (rgb 227 182 104) |> move (-87,-5)
              , openPolygon [(-0.5,-9),(7,-9)] |> outlined (solid 0.8) (rgb 227 182 104) |> move (-87,-5)
              , openPolygon [(6,-9),(7,-10)] |> outlined (solid 0.8) (rgb 227 182 104) |> move (-87,-5)
              , openPolygon [(0,0),(-5,0)] |> outlined (solid 0.3) black |> move (-66,-40)
              , openPolygon [(0,-23),(-5,-23)] |> outlined (solid 0.3) black |> move (-66,-40)
              , openPolygon [(0,-11.5),(-5,-11.5)] |> outlined (solid 0.3) black |> move (-66,-40)
              , openPolygon [(0,11.5),(-5,11.5)] |> outlined (solid 0.3) black |> move (-66,-40)
              , openPolygon [(0,23),(-5,23)] |> outlined (solid 0.3) black |> move (-66,-40)
              , text "10m"  |> filled black |> scale 0.4 |> move (-80,-41)
              , text "0m"  |> filled black |> scale 0.4 |> move (-79,-63.5)
              ]
        
wave amplitude = group [curve (0,0) [Pull (10,amplitude) (20,0)] |> filled (rgb 52 110 235) |> move (-65,-40)
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> filled (rgb 156 215 255) |> rotate (degrees 180) |> move (-25,-40)
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> filled (rgb 52 110 235) |> move (-25,-40)
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> filled (rgb 156 215 255) |> rotate (degrees 180) |> move (15,-40)
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> filled (rgb 52 110 235) |> move (15,-40)
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> filled (rgb 156 215 255) |> rotate (degrees 180) |> move (55,-40)
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> filled (rgb 52 110 235) |> move (55,-40)
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> filled (rgb 156 215 255) |> rotate (degrees 180) |> move (95,-40)
                        -- outlines
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> outlined (solid 1) black |> move (-65,-40)
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> outlined (solid 1) black |> rotate (degrees 180) |> move (-25,-40)
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> outlined (solid 1) black |> move (-25,-40)
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> outlined (solid 1) black |> rotate (degrees 180) |> move (15,-40)
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> outlined (solid 1) black |> move (15,-40)
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> outlined (solid 1) black |> rotate (degrees 180) |> move (55,-40)
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> outlined (solid 1) black |> move (55,-40)
                        , curve (0,0) [Pull (10,amplitude) (20,0)] |> outlined (solid 1) black |> rotate (degrees 180) |> move (95,-40)
                       ]
water = rect 160 30 |> filled (rgb 52 110 235) |> move (15,-55.05)

equation amplitude = group [text "y = " |> filled black |> scale 0.6 |> move (-14,10)
                           , text amplitude |> filled black |> scale 0.6 |> move (-3,10) 
                           , if (amplitude == "10") then text "sin (x) + 10" |> filled black |> scale 0.6 |> move (5,10) else text "sin (x) + 10" |> filled black |> scale 0.6 |> move (3,10) 
                           ]

type DragState = Released | Dragging -- Defines DragState

type Msg = Tick Float GetKeyState | ChangeDragStateDown (Float, Float) | ChangeDragStateUp (Float, Float) | ChangePos (Float, Float) | ChangeDragStateLeave

update msg model = case msg of
                    Tick t _ -> { model | time = t }
                     
                    ChangeDragStateDown (_,y) -> { model | dragState = case model.dragState of
                                                                        Released -> Dragging
                                                                        Dragging -> Dragging
                                                                    ,pos = (Tuple.first model.pos, y)
                                                                    }

                    ChangeDragStateUp (_,y) -> { model | dragState = case model.dragState of
                                                                        Released -> Released
                                                                        Dragging -> Released
                                                                    ,pos = (Tuple.first model.pos, y)
                                                                    }

                    ChangeDragStateLeave -> { model | dragState = case model.dragState of
                                                                        Released -> Released
                                                                        Dragging -> Released
                                                                    }
                    
                    ChangePos (_,y) -> { model | pos = if (model.dragState == Dragging) then (Tuple.first model.pos, y) else model.pos} 


init = { time = 0, pos = (-90, 5), dragState = Released }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)