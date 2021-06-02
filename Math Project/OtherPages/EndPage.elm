module EndPage exposing (main)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (GetKeyState, gameApp)

import Html
import Html.Attributes exposing (..)
import Bootstrap.Button as Button

myShapes model = [ -- The text and Congradulations image is animated using sin equations and model.time
                   imageFit "http://localhost:8000/Math%20Project/Images/congratulationsBanner.jpg" 534 160 |> scale 0.2 |> move (5*(sin(model.time*3)) - 50,63)
                 , text "You Finished the Quiz" |> centered |> filled (rgb (255*sin(3.14*(model.time/10))) (255*cos(3.14*(model.time/10) + 1/3))  (255*sin(3.14*(model.time/10) + 2/3))) |>scale 0.7 |> move (5*(sin(model.time*5)),23) 
                 
                 -- Displays Award Image
                 , imageFit "http://localhost:8000/Math%20Project/Images/Award.png" 438 800 |> scale 0.08 |> move (-15, 20) 
                 
                 -- Creates Main Menu Button Using the buttonLink function
                 , buttonLink "Return to Main Menu" "http://localhost:8000/Math%20Project/Home.elm" "black" |>scale 0.75 |> move (50,-50)
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