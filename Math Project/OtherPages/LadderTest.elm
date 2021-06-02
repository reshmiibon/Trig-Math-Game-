module LadderTest exposing (main)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (GetKeyState, gameApp)
import Html
import Html.Attributes exposing (..)
import Bootstrap.Button as Button
import Round

-- Contains shapes,buttons and text to create ladder trig application for test
myShapes model = let 
                    mcMoves =  round (toRandom model.randTime 1 4) -- Uses a random number to asign mutiple choice button positions
                  in
                  
                  (if (model.randTime <= 0) then [] else
                  [
                  rect 200 220 |> filled (rgb 156 215 255) --Displays blue background (sky) 
                  ,ground 
                  ,wall
                      |>move((adjacent_length model),0)
                  ,ladder |>transform (rotateAboutPoint model)
                  --Text for angle between ground and ladder 
                  ,text (String.concat ["θ = ",(theta model),"°"])  
                  |> filled red
                  |>move(-90,-88)
                  |>scale 0.5
                  --Text for ladder length 
                  , text "Ladder length = 60m" 
                  |> filled darkBlue
                  |>move(-185,-50)
                  |>scale 0.5
                  ,rect 10 8
                  |> filled black
                  |>move(-105,-108),
                  --Text for ground length (unknown labelled as x)
                  text "x=?"
                  |>filled darkBlue
                  |>scale 0.5
                  |>move ((adjacent_length model)-40,-50)
                
                  -- Creates Quit Button Using the buttonLink function
                  , buttonLink "Quit" "http://localhost:8000/Math%20Project/Home.elm" "black" |>scale 0.75 |> move (50,-50)
                  
                  -- Generates Mutiple choice buttions randomizing other numbers and there postions
                  , text "What is the length along the ground?" |> alignLeft |> filled black |> scale 0.4 |> move (-95,45)
                  , buttonMC ("x = "++ (Round.round 2 (adjacent_length model)) ++"m")  (if (model.winState1 == Pass) then green else if (model.winState1 == Fail) then red else black) |> move (if (mcMoves == 1) then (-75,37) else if (mcMoves == 2) then (-75,27) else if (mcMoves == 4) then (-75,7) else (-75,17)) |> notifyTap Button1
                  , buttonMC ("x = "++ (Round.round 2 (toRandom model.randTime 1 50)) ++"m") (if (model.winState2 == Pass) then green else if (model.winState2 == Fail) then red else black) |> move (if (mcMoves == 2) then (-75,17) else (-75,27)) |> notifyTap Button2
                  , buttonMC ("x = "++ (Round.round 2 (toRandom model.randTime 10 300)) ++"m")  (if (model.winState3 == Pass) then green else if (model.winState3 == Fail) then red else black) |> move (if (mcMoves == 1) then (-75,17) else (-75,37)) |> notifyTap Button3
                  , buttonMC ("x = "++ (Round.round 2 (toRandom model.randTime 20 70)) ++"m") (if (model.winState4 == Pass) then green else if (model.winState4 == Fail) then red else black) |> move (if (mcMoves == 4) then (-75,17) else (-75,7)) |> notifyTap Button4
                  ] -- Shows the Next Button if you get the correct answer
                    ++ (if (model.winStateFinal == Pass) then [ buttonLink "Next" "http://localhost:8000/Math%20Project/OtherPages/PlaneTest.elm" "black" |>scale 0.75 |> move (65,-50)] else [])
                    
                    -- Displays hint and hint Button
                    ++ (if (model.hintState == False) then [ buttonHint |> scale 0.75 |> move (40,-55) |> notifyTap Hint] 
                       else [ imageFit "http://localhost:8000/Math%20Project/Images/LadderHint.jpg" 1006 502 |> scale 0.07 |> move (15, 55)
                            , rect 1006 502 |> outlined (solid 10) black |> scale 0.07 |> move (50, 37.5)]))

--Grouped shapes to make code clearer and easier to use the distinct shapes 
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

ident = ( ( 1 , 0 , 0 ) , ( 0 , 1 , 0 ) ) --Identity matrix means no transformation yet  

 -- Calculations for adjacent side, opposite side and angle (theta)
adjacent_length model = cos(degrees (toFloat(round (toRandom model.randTime 1 89))))*60
opp_length model = sin(degrees (toFloat(round (toRandom model.randTime 1 89))))*60
theta model = String.fromInt(round (toRandom model.randTime 1 89))

-- Transforms the indentity matrix by rotation and is applied to the ladder shape
rotateAboutPoint model  =
    ident
     |> rotateAboutT (-20, -45) (degrees (toFloat(round (toRandom model.randTime 1 89))))  

-- Converts short pseudo random delay into pseudo random number within given range
toRandom x max min = (abs (sin (100000*3.14*x)))*(max-min) + min

-- Generates mutiple choice buttons
buttonMC inText inColor = group [ rect 45 10 |> filled darkGrey
                                , rect 45 10 |> outlined (solid 1) inColor
                                , text inText |> alignLeft |> filled black |> scale 0.5 |> move (-19, -2)
                                  ]|> scale 0.8
-- Generates hint buttons
buttonHint  = group [ rect 25 10 |> filled grey
                                , rect 25 10 |> outlined (solid 0.5) black
                                , text "Hint" |> centered |> filled black |> scale 0.6 |> move (0, -2)
                                  ]|> scale 0.8

-- Uses Html to create a linked Html Button
buttonLink inText inLink inTextColor  = ( html 1000 1000 <| Button.button [ Button.primary] [Html.a [target "", href inLink] [Html.h1 [style "text-decoration" "none", style "color" inTextColor] [ Html.text inText]]]) |> scale 0.2
-- Uses Html to create a Text Link 
link inText inLink inTextColor = ( html 1000 100 <| Html.a [target "", href inLink] [Html.h5 [style "text-decoration" "none", style "color" inTextColor] [ Html.text inText]] ) |> scale 0.4

image inLink = ( html 1000 667 <| Html.img [src inLink] [] ) -- Uses Html to generate a fixed size image
imageFit inLink x y = ( html x y <| Html.img [src inLink] [] ) -- Uses Html to generate a custom resulution image

-- buttonLink2 inText inLink = ( html 1000 667 <| Button.button [ Button.primary, Button.onClick   ] [ Html.text inText]) 

type WinState = Pass | Fail | None -- Decalres WinState

type Msg = Tick Float GetKeyState | Button1 | Button2 | Button3 | Button4 | Hint

waitFunction1 x = if (x <= 0) then 0 else waitFunction1 (x-1) -- Creates short pseudo random delay
waitFunction2 x = if (x <= 0) then 0 else waitFunction2 (x-2) -- Creates short pseudo random delay

update msg model = case msg of
                  Tick t _ -> { model| time = t
                                , wait =  if (model.wait > 0) then (waitFunction1 model.wait + waitFunction2 model.wait) else model.wait
                                , randTime = if (model.randTime == 0 && model.wait <= 0) then model.time else model.randTime  }
                  
                  -- Handles mutiple choice button one click (correct answer)
                  Button1 -> { model |  winState1 = if (model.winStateFinal == None) then Pass else model.winState1
                                                        , winStateFinal = if (model.winStateFinal == None) then Pass else model.winStateFinal           
                                                                   }
                  -- Handles mutiple choice button two click
                  Button2 -> { model |  winState2 = if (model.winStateFinal == None) then Fail else model.winState2
                                                                    }
                  -- Handles mutiple choice button three click
                  Button3 -> { model |  winState3 = if (model.winStateFinal == None) then Fail else model.winState3
                                                     
                                                                    }
                  -- Handles mutiple choice button four click
                  Button4 -> { model |  winState4 = if (model.winStateFinal == None) then Fail else model.winState4
                                                                    }
                  -- Handles hint button being clicked
                  Hint -> { model |  hintState = True }
                     
                    

init = { time = 0, wait = 1000, randTime = 0.0,  winState1 = None, winState2 = None, winState3 = None, winState4 = None, winStateFinal = None, hintState = False, angleC = 50, angleB = 30, angleA = 100, sideb = (((sin (degrees 30))*(80))/(sin (degrees 100))), sidec = (((sin (degrees 50))*(80))/(sin (degrees 100))) }


main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)