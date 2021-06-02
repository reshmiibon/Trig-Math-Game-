module Plane exposing (main)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (GetKeyState, gameApp)

import Html
import Html.Attributes exposing (..)
import Bootstrap.Button as Button
import Round


myShapes model = let
                  -- Updates the side lengths and angles using a random number generated at startup
                  angleC = (round (toRandom model.randTime 20 60)) |> toFloat 
                  angleA = 100
                  angleB = 180 - angleA - angleC
                  sideb = (((sin (degrees angleB))*(80))/(sin (degrees angleA)))
                  sidec = (((sin (degrees angleC))*(80))/(sin (degrees angleA)))

                  -- Uses a random number to asign mutiple choice button positions
                  mcMoves =  round (toRandom model.randTime 1 4)

                  in
                  
                  (if (model.randTime <= 0) then [] else
                  [rect 200 220 |> filled (rgb 150 210 230) -- Background

                  -- Creates Tower and Plane Images
                  ,image "http://localhost:8000/Math%20Project/Images/Tower.png"|> scale 0.035 |> move (-58,-27)
                  , image "http://localhost:8000/Math%20Project/Images/Tower.png"|> scale 0.035 |> move (40,-27)
                  , imageFit "http://localhost:8000/Math%20Project/Images/Plane.png" 1280 640 |> scale 0.03 |> move ((cos (degrees angleC))*sideb - 62, (sin (degrees angleC))*sideb - 22)
                  
                  
                  , rect 200 40 |> filled darkGray |> move (0,-64)-- Ground Shape

                  -- Draws Polygon Connecting the plane to the towers
                  ,openPolygon [(-40,-43),((cos (degrees angleC))*sideb - 40, (sin (degrees angleC))*sideb - 43),(40,-43)] |> outlined (dotted 0.5) black 
                  
                  -- Displays angles A, B, C and side b, a
                  , text "a = 80m" |> centered |> filled black  |> scale 0.5 |> move (0,-50)
                  ,text ("C = " ++ (Round.round 0 angleC) ++ "°") |> alignLeft |> filled black  |> scale 0.5 |> move (-80,-42)
                  ,text ("B = " ++ (Round.round 0 angleB)  ++ "°") |> alignRight |> filled black  |> scale 0.5 |> move (80,-42)
                  ,text ("A = " ++ (Round.round 0 angleA)  ++ "°") |> centered |> filled black  |> scale 0.5 |> move ((cos (degrees angleC))*sideb - 40, (sin (degrees angleC))*sideb - 42)
                  ,text ("b = ?") |> centered |> filled black  |> scale 0.5 |> move (0,1) |> rotate (degrees angleC) |> move ((cos (degrees angleC))*(sideb/2) - 40, (sin (degrees angleC))*(sideb/2) - 43)
                  
                  -- Creates Quit Button Using the buttonLink function
                  , buttonLink "Quit" "http://localhost:8000/Math%20Project/Home.elm" "black" |>scale 0.75 |> move (50,-50)

                  -- Generates Mutiple choice buttions randomizing other numbers and there postions
                  , text "What is the length of side b" |> alignLeft |> filled black |> scale 0.5 |> move (-95,45)
                  , buttonMC ("b = "++ (Round.round 2 (toRandom model.randTime 10 300)) ++"m") (if (model.winState1 == True) then red else black) |> move (if (mcMoves == 1) then (-75,17) else (-75,37)) |> notifyTap Button1
                  , buttonMC ("b = "++ (Round.round 2 (toRandom model.randTime 1 50)) ++"m") (if (model.winState2 == True) then red else black) |> move (if (mcMoves == 2) then (-75,17) else (-75,27)) |> notifyTap Button2
                  , buttonMC ("b = "++ (Round.round 2 sideb) ++"m") (if (model.winState3 == True) then green else black) |> move (if (mcMoves == 1) then (-75,37) else if (mcMoves == 2) then (-75,27) else if (mcMoves == 4) then (-75,7) else (-75,17)) |> notifyTap Button3
                  , buttonMC ("b = "++ (Round.round 2 (toRandom model.randTime 20 70)) ++"m") (if (model.winState4 == True) then red else black) |> move (if (mcMoves == 4) then (-75,17) else (-75,7)) |> notifyTap Button4


                  ] -- Shows the Next Button if you get the correct answer
                    ++ (if (model.winStateFinal == Pass) then [ buttonLink "Next" "http://localhost:8000/Math%20Project/OtherPages/TidesTest.elm" "black" |>scale 0.75 |> move (65,-50)] else [])
                    
                    -- Displays hint and hint Button
                    ++ (if (model.hintState == False) then [ buttonHint |> scale 0.75 |> move (40,-55) |> notifyTap Hint] 
                       else [imageFit "http://localhost:8000/Math%20Project/Images/SineLaw.jpg" 945 576 |> scale 0.08 |> move (15, 55)
                            , rect 945 576 |> outlined (solid 10) black |> scale 0.08 |> move (53, 32)]))

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

type WinState = Pass | Fail | None -- Decalres WinState

type Msg = Tick Float GetKeyState | Button1 | Button2 | Button3 | Button4 | Hint 


waitFunction1 x = if (x <= 0) then 0 else waitFunction1 (x-1) -- Creates short pseudo random delay
waitFunction2 x = if (x <= 0) then 0 else waitFunction2 (x-2) -- Creates short pseudo random delay

update msg model = case msg of
                    Tick t _ -> { model| time = t
                                , wait =  if (model.wait > 0) then (waitFunction1 model.wait + waitFunction2 model.wait) else model.wait -- Trigers short pseudo random delay on startup
                                , randTime = if (model.randTime == 0 && model.wait <= 0) then model.time else model.randTime } -- Times short pseudo random delay

                    -- Handles mutiple choice button one click
                    Button1 -> { model |  winState1 = if (model.winStateFinal == None) then True else model.winState1
                                                                    }
                    -- Handles mutiple choice button two click
                    Button2 -> { model |  winState2 = if (model.winStateFinal == None) then True else model.winState2
                                                                    }
                    -- Handles mutiple choice button three click (Correct Answer)
                    Button3 -> { model |  winState3 = if (model.winStateFinal == None) then True else model.winState3
                                        , winStateFinal = if (model.winStateFinal == None) then Pass else model.winStateFinal                           
                                                                    }
                    -- Handles mutiple choice button four click
                    Button4 -> { model |  winState4 = if (model.winStateFinal == None) then True else model.winState4
                                                                    }

                    -- Handles hint button being clicked
                    Hint -> { model |  hintState = True }
                     
                    

init = { time = 0, wait = 1000, randTime = 0.0, winState1 = False, winState2 = False, winState3 = False, winState4 = False, winStateFinal = None, hintState = False }


main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)