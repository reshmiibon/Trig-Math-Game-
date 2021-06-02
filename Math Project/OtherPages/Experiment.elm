module Experiment exposing (main)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (GetKeyState, gameApp)

import Html
import Html.Attributes exposing (..)
import Bootstrap.Button as Button

myShapes model = [ rect 200 220 |> filled (rgb 176 224 230)
                 , rect 200 220 |> filled (rgb 156 215 255) |> move (-3,40)
                 , visuals model 
                 , text "Experiment with Trig Applications"  |> centered |> filled (rgb (30*model.time) 50 255)  |> scale 1.1 |> move (0,45) --|> move (5*(sin(model.time*5)),45) 
                 , buttonLink "Ladder Example" "http://localhost:8000/Math%20Project/OtherPages/Ladder.elm" "green" |>scale 1 |> move (-20,30) 
                 , buttonLink "Tides Example" "http://localhost:8000/Math%20Project/OtherPages/Tides.elm" "purple" |>scale 1 |> move (-18,10)
                 , buttonLink "Plane Example" "http://localhost:8000/Math%20Project/OtherPages/Plane.elm" "darkRed" |>scale 1 |> move (-18,-10)
                 , buttonLink "Back" "http://localhost:8000/Math%20Project/Home.elm" "black" |>scale 0.75 |> move (65,-50)
                   ]

visuals model = group [ rect 270 40 |> filled lightGreen |> move (0,-20)
                , rect 270 40 |> filled (rgb 242 209 107) |> move (0,-50)
                , imageFit "http://localhost:8000/Math%20Project/Images/examplePage1.png" 700 910 |> move (-430,200) |> scale 0.13
                , imageFit "http://localhost:8000/Math%20Project/Images/examplePage2.png" 3560 770 |> move (-1900,-300) |> scale 0.061 |> move (3*(sin(model.time*5)),(cos(model.time*2)))
                , imageFit "http://localhost:8000/Math%20Project/Images/Tower.png" 700 910 |> move (-2730,400) |> scale 0.03
                , imageFit "http://localhost:8000/Math%20Project/Images/Plane.png" 1280 640 |> scale 0.03 |> move (40,25) |> rotate (degrees 15)
                , rect 2 8 |> filled red |> move (0,38)
                , triangle 3 |> filled red |> rotate (degrees -90) |> move (0,35)
                ]

buttonLink inText inLink inTextColor  = ( html 1000 1000 <| Button.button [ Button.primary] [Html.a [target "", href inLink] [Html.h1 [style "text-decoration" "underline", style "color" inTextColor] [ Html.text inText]]]) |> scale 0.2
link inText inLink inTextColor = ( html 1000 100 <| Html.a [target "", href inLink] [Html.h5 [style "text-decoration" "none", style "color" inTextColor] [ Html.text inText]] ) |> scale 0.4

image inLink = ( html 1000 667 <| Html.img [src inLink] [] ) 
imageFit inLink x y = ( html x y <| Html.img [src inLink] [] ) 


type Msg = Tick Float GetKeyState

update msg model = case msg of
                     Tick t _ -> { model| time = t }

init = { time = 0 }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)