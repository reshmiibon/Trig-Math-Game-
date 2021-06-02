module Experiment exposing (main)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (GetKeyState, gameApp)

import Html
import Html.Attributes exposing (..)
import Bootstrap.Button as Button

myShapes model = [ rect 200 220 |> filled (rgb 255 193 91) -- Background
                 -- The text and Congradulations image is animated using sin equations and model.time
                 , text "Some Useful Formulas" |> centered |> filled (rgb (255*cos(3.14*(model.time/5))) 0 (255*sin(3.14*(model.time/5))))  |>scale 1 |> move (5*(sin(model.time*5)),45)
                 
                 -- Generates Formula Images 
                 , imageFit "http://localhost:8000/Math%20Project/Images/SineLaw.png" 945 576 |> scale 0.1 |> move (-90, 45)
                 , imageFit "http://localhost:8000/Math%20Project/Images/LadderHint.png" 999 512 |> scale 0.1 |> move (-90, -7)
                 , imageFit "http://localhost:8000/Math%20Project/Images/SinWave.svg" 355 215 |> scale 0.23 |> move (8, 25)
                 
                 -- Creates Back Button Using the buttonLink function
                 , buttonLink "Back" "http://localhost:8000/Math%20Project/Home.elm" "black" |>scale 0.75 |> move (65,-50)
                   ]

-- Uses Html to create a linked Html Button
buttonLink inText inLink inTextColor  = ( html 1000 1000 <| Button.button [ Button.primary] [Html.a [target "", href inLink] [Html.h1 [style "text-decoration" "none", style "color" inTextColor] [ Html.text inText]]]) |> scale 0.2
-- Uses Html to create a Text Link 
link inText inLink inTextColor = ( html 1000 100 <| Html.a [target "", href inLink] [Html.h5 [style "text-decoration" "none", style "color" inTextColor] [ Html.text inText]] ) |> scale 0.4 

image inLink = ( html 1000 667 <| Html.img [src inLink] [] ) -- Uses Html to generate a fixed size image
imageFit inLink x y = ( html x y <| Html.img [src inLink] [] ) -- Uses Html to generate a custom resulution image


type Msg = Tick Float GetKeyState

update msg model = case msg of
                     Tick t _ -> { model| time = t }

init = { time = 0 }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)