module Ladder exposing (main)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (GetKeyState, gameApp)
import Round --in terminal: elm install myrho/elm-round
import Html
import Html.Attributes exposing (..)
import Bootstrap.Button as Button 

-- Contains shapes,buttons and text to create ladder trig application 
myShapes model = [   
    rect 200 220 |> filled (rgb 156 215 255) -- Displays the blue background (sky) 
    ,
    ground -- Displays the ground 
    ,
    wall
    |>move((adjacent_length model),0) --Displays the red wall 
    ,
    ladder |>transform (rotateAboutPoint model) --Displays the ladder which rotates as angle changes 
    ,
    text (Debug.toString (adjacent_length model)) 
    |> filled black
    |> move(-85,-80)
    ,
    text (String.concat ["θ = ",(theta model),"°"]) --Text for angle between ground and ladder 
    |> filled red
    |>move(-85,-85)
    |>scale 0.5
    ,
    --Text for ground length including calculations 
    text (String.concat ["Length along ground = Cos(θ)*Hypotenuse = Cos(",(theta model),"°)*60 = ", Round.round (1) (adjacent_length model),"m"]) --text for ground length 
    |> filled darkBlue
    |>move(-188,-110)
    |>scale 0.5
    ,
    --Text for wall length including calculations 
    text ("Length along wall") 
    |> filled darkBlue
    |>move(-155,33)
    |>scale 0.5
    ,
    text ("= Sin(θ)*Hypotenuse") 
    |> filled darkBlue
    |>move(-155,15)
    |>scale 0.5
    ,
    text (String.concat ["= Sin(",(theta model),"°)*60"])
    |> filled darkBlue
    |>move(-155,-5)
    |>scale 0.5
    ,
    text (String.concat ["= ",Round.round (1) (opp_length model),"m"])
    |> filled darkBlue
    |>move(-155,-25)
    |>scale 0.5
    ,
    --Text for ladder length 
    text "Ladder length = 60m" 
    |> filled darkBlue
    |>move(-155,-65)
    |>scale 0.5,
    --Title text 
    text ("Ladder Application using")
     |>outlined (solid 1) darkPurple
     |>move(-156,85)
     |>scale 0.6,
     text ("SOH CAH TOA")
     |>outlined (solid 1) orange
     |>move(-152,70)
     |>scale 0.57

     -- Creates Back Button Using the buttonLink function
    , buttonLink "Back" "http://localhost:8000/Math%20Project/OtherPages/Experiment.elm" "black" |>scale 0.75 |> move (70,-50)
    ] ++ slider model model.pos -- Displays the slider 

-- Uses Html to create a linked Html Button
buttonLink inText inLink inTextColor  = ( html 1000 1000 <| Button.button [ Button.primary] [Html.a [target "", href inLink] [Html.h1 [style "text-decoration" "none", style "color" inTextColor] [ Html.text inText]]]) |> scale 0.2

--grouped shapes to make code clearer and easier to use the distinct shapes 
ground= group[
             rect 200 30 --width,height
             |> filled lightGreen
             |> move (0,-60) ]
             
wall= group[
             rect 40 200 --width,height
             |> filled (rgb 179 0 45)
             |> move (0,55) ]         
                     
ladder = group[
            openPolygon [(-20,-42),(40,-42),(40,-46),(-20,-46)]
              |> outlined (solid 1) black,
            rect 1 4
             |> filled black
              |> move (-18,-44.5),
             rect 1 4
              |> filled black
              |> move (-12,-44.5),
              rect 1 4
              |> filled black
              |> move (-6,-44.5),
              rect 1 4
              |> filled black
              |> move (0,-44.5),
              rect 1 4
              |> filled black
              |> move (6,-44.5),
              rect 1 4
              |> filled black
              |> move (12,-44.5),
              rect 1 4
              |> filled black
              |> move (18,-44.5),
              rect 1 4
              |> filled black
              |> move (24,-44.5),
              rect 1 4
              |> filled black
              |> move (30,-44.5),
              rect 1 4
              |> filled black
              |> move (36,-44.5),
              rect 1 3
              |> filled (rgb 156 215 255)
              |> move (40,-44)
              ]
 -- Calculations for adjacent side, opposite side and angle (theta)
adjacent_length model = cos(degrees (toFloat(round((((Tuple.second model.pos) + 43.5)*(86.9/84))))))*60
opp_length model = sin(degrees (toFloat(round((((Tuple.second model.pos) + 43.5)*(86.9/84))))))*60
theta model = String.fromInt(round (((Tuple.second model.pos) + 43.5)*(86.9/84)))

-- Generates slider
slider model sliderpos = [rect 5 100 |> filled gray |> move (85,0), 
                          circle 5 |> filled gray |> move sliderpos,
                          circle 5 |> outlined (solid 1) black |> move sliderpos,
                         text (Debug.toString (round (((Tuple.second sliderpos) + 43.5)*(86.9/84)))) |> filled black |> scale 0.5 |> move (((Tuple.first sliderpos) - 3), ((Tuple.second sliderpos) - 2)) ] ++
                          if (model.pos == sliderpos) then [rect 20 85 |> ghost |> move (85,0) |> notifyMouseDownAt ChangeDragStateDown |> notifyMouseUpAt ChangeDragStateUp |> notifyLeave ChangeDragStateLeave |> notifyMouseMoveAt ChangePos] else []

-- Transform type used when rotating about a specified point 
type alias Transform =
    ( ( Float, Float, Float ), ( Float, Float, Float ) )

ident = ( ( 1 , 0 , 0 ) , ( 0 , 1 , 0 ) ) --identity matrix means no transformations applied yet  

-- Transforms the indentity matrix by rotation and is applied to the ladder shape
rotateAboutPoint model  =
    ident
     |> rotateAboutT (-20, -45)  (degrees (((Tuple.second model.pos) + 43.5)*(86.9/84))) 

type DragState = Released | Dragging -- Defines DragState

type Msg = Tick Float GetKeyState | ChangeDragStateDown (Float, Float) | ChangeDragStateUp (Float, Float) | ChangePos (Float, Float) | ChangeDragStateLeave

update msg model = case msg of
                    Tick t _ -> { model| time = t  }
                    
                    -- Handles the condition when slider one is clicked down
                    ChangeDragStateDown (_,y) -> { model | dragState = case model.dragState of
                                                                        Released -> Dragging
                                                                        Dragging -> Dragging
                                                                    ,pos = (Tuple.first model.pos, y)
                                                                    }
                    -- Handles the condition when slider is relased
                    ChangeDragStateUp (_,y) -> { model | dragState = case model.dragState of
                                                                        Released -> Released
                                                                        Dragging -> Released
                                                                    ,pos = (Tuple.first model.pos, y)
                                                                    }
                    -- Handles the condition when slider is moved off of while draging
                    ChangeDragStateLeave -> { model | dragState = case model.dragState of
                                                                        Released -> Released
                                                                        Dragging -> Released
                                                                    }
                    -- Updates slider's position
                    ChangePos (_,y) -> { model | pos = if (model.dragState == Dragging) then (Tuple.first model.pos, y) else model.pos} 


init = { time = 0, pos = (85, 10), dragState = Released }
main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }
view model = collage 192 128 (myShapes model)