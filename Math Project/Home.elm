module Home exposing (main)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (GetKeyState, gameApp)

import Html
import Html.Attributes exposing (..)
import Bootstrap.Button as Button

myShapes model = [  rect 200 220 |> filled (rgb 176 224 230), -- Background (blue color)
                  --Images for home page (link to locally saved images in Images folder)
                  imageFit "http://localhost:8000/Math%20Project/Images/trig2.jpeg" 534 300 |> scale 0.2 |> move (-85, 15) |>rotate (degrees 5) ,
                  imageFit "http://localhost:8000/Math%20Project/Images/trig1.jpeg" 520 450 |> scale 0.1 |> move (42,15) |>rotate (degrees -5)
                  --Title for home page 
                 , text "Welcome to Trig Math Game!" |> centered |> filled (rgb (255*sin(3.14*(model.time/10))) 50 255)  |>scale 1.2 |> move (5*(sin(model.time*5)),45) 
                  --3 buttons which link to 3 pages: Trig application examples, test and formula pages
                 , buttonLink "Experiment with Trig Applications" "http://localhost:8000/Math%20Project/OtherPages/Experiment.elm" "green" |>scale 1 |> move (-43,30) 
                 , buttonLink "Test Your Knowledge" "http://localhost:8000/Math%20Project/OtherPages/LadderTest.elm" "purple" |>scale 1 |> move (-27,5)
                 , buttonLink "Useful Formulas" "http://localhost:8000/Math%20Project/OtherPages/Formulas.elm" "darkRed" |>scale 1 |> move (-20,-20)
                   ]

-- Uses Html to create a linked Html Button
buttonLink inText inLink inTextColor  = ( html 1000 1000 <| Button.button  [ Button.success]    [Html.a [target "", href inLink] [Html.h1 [style "text-decoration" "underline", style "color" inTextColor] [ Html.text inText]]]) |> scale 0.2 
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