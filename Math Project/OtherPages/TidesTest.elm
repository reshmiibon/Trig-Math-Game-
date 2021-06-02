module TidesTest2 exposing (main)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (GetKeyState, gameApp)

import Html
import Html.Attributes exposing (..)
import Bootstrap.Button as Button
import Round


myShapes model = let
                    mcMoves =  round (toRandom model.randTime 1 4)
                    ampInt = round (toRandom model.randTime 1 11)
                    ampFloat = (toRandom model.randTime 1 11)
                    ampString = String.fromInt ampInt
                    question = "If the amplitude is " ++ ampString ++ ", what is the maximum height of the wave?"
                  in
                    (if (model.randTime <= 0) then [] else
                  [ rect 200 220 |> filled (rgb 156 215 255) |> move (-3,40)
                  , beach
                  , water
                  , wave ((((Tuple.second model.pos) + 0.05)*(10/55) * 3 ) + 20)
                  , text "Wave Equation:" |> filled black |> scale 0.8 |> move (-20,20)
                  , equation ampString 
                  
                  , text question |> alignLeft |> filled black |> scale 0.4 |> move (-93,50)
                  , buttonMC ("y = "++ (Round.round 0 (toRandom model.randTime 5 8)) ++" m") (if (model.winState1 == True) then red else black) |> move (if (mcMoves == 1) then (-75,17) else (-75,37)) |> notifyTap Button1
                  , buttonMC ("y = "++ (Round.round 0 (toRandom model.randTime 1 3)) ++" m") (if (model.winState2 == True) then red else black) |> move (if (mcMoves == 2) then (-75,17) else (-75,27)) |> notifyTap Button2
                  , buttonMC ("y = "++ (Round.round 0 (ampFloat + 10)) ++" m") (if (model.winState3 == True) then green else black) |> move (if (mcMoves == 1) then (-75,37) else if (mcMoves == 2) then (-75,27) else if (mcMoves == 4) then (-75,7) else (-75,17)) |> notifyTap Button3
                  , buttonMC ("y = "++ (Round.round 0 (toRandom model.randTime 9 11)) ++" m") (if (model.winState4 == True) then red else black) |> move (if (mcMoves == 4) then (-75,17) else (-75,7)) |> notifyTap Button4


                  , buttonLink "Quit" "http://localhost:8000/Math%20Project/Home.elm" "black" |>scale 0.75 |> move (50,-50)


                  ] ++ (if (model.winStateFinal == Pass) then [ buttonLink "Next" "http://localhost:8000/Math%20Project/OtherPages/EndPage.elm" "black" |>scale 0.75 |> move (65,-50)] else [])
                    ++ (if (model.hintState == False) then [ buttonHint |> scale 0.75 |> move (40,-55) |> notifyTap Hint] 
                       else [imageFit "http://localhost:8000/Math%20Project/Images/SinWave.svg" 1024 576 |> scale 0.15 |> move (40, 55)]))

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


toRandom x max min = (abs (sin (100000*3.14*x)))*(max-min) + min

buttonMC inText inColor = group [ rect 45 10 |> filled darkGrey
                                , rect 45 10 |> outlined (solid 1) inColor
                                , text inText |> alignLeft |> filled black |> scale 0.5 |> move (-19, -2)
                                  ]|> scale 0.8



buttonHint  = group [ rect 25 10 |> filled grey
                                , rect 25 10 |> outlined (solid 0.5) black
                                , text "Hint" |> centered |> filled black |> scale 0.6 |> move (0, -2)
                                  ] |> scale 0.8

buttonLink inText inLink inTextColor  = ( html 1000 1000 <| Button.button [ Button.primary] [Html.a [target "", href inLink] [Html.h1 [style "text-decoration" "none", style "color" inTextColor] [ Html.text inText]]]) |> scale 0.2
link inText inLink inTextColor = ( html 1000 100 <| Html.a [target "", href inLink] [Html.h5 [style "text-decoration" "none", style "color" inTextColor] [ Html.text inText]] ) |> scale 0.4

image inLink = ( html 1000 667 <| Html.img [src inLink] [] ) 
imageFit inLink x y = ( html x y <| Html.img [src inLink] [] ) 

-- buttonLink2 inText inLink = ( html 1000 667 <| Button.button [ Button.primary, Button.onClick   ] [ Html.text inText]) 

type WinState = Pass | Fail | None 

type Msg = Tick Float GetKeyState | Button1 | Button2 | Button3 | Button4 | Hint 


waitFunction1 x = if (x <= 0) then 0 else waitFunction1 (x-1) 
waitFunction2 x = if (x <= 0) then 0 else waitFunction2 (x-2) 

update msg model = case msg of
                    Tick t _ -> { model| time = t
                                , wait =  if (model.wait > 0) then (waitFunction1 model.wait + waitFunction2 model.wait) else model.wait
                                , randTime = if (model.randTime == 0 && model.wait <= 0) then model.time else model.randTime  }

                    Button1 -> { model |  winState1 = if (model.winStateFinal == None) then True else model.winState1
                                                                    }
                    Button2 -> { model |  winState2 = if (model.winStateFinal == None) then True else model.winState2
                                                                    }
                    Button3 -> { model |  winState3 = if (model.winStateFinal == None) then True else model.winState3
                                        , winStateFinal = if (model.winStateFinal == None) then Pass else model.winStateFinal                           
                                                                    }
                    Button4 -> { model |  winState4 = if (model.winStateFinal == None) then True else model.winState4
                                                                    }

                    Hint -> { model |  hintState = True }
                     
                    

init = { time = 0,  pos = (-90, 5), wait = 1000, randTime = 0.0, winState1 = False, winState2 = False, winState3 = False, winState4 = False, winStateFinal = None, hintState = False }


main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)