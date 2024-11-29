module Main exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Random as R exposing (..)


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type alias Model = 
                 {myBoard : Board
                 ,playingAs : MaybeShape
                 }



type alias Board =
  { rowOne   : Array String
  , rowTwo   : Array String
  , rowThree : Array String
  }


init : () -> (Model, Cmd Msg)
init _ = ({myBoard = initialBoard, playingAs = Nothing}, Cmd.none)

initialBoard = 
           {rowOne =   fromList ["","",""]
           ,rowTwo =   fromList ["","",""]
           ,rowThree = fromList ["","",""]
           }
updateBoard : Shape -> (Int,Int) -> Board -> Board 
updateBoard s (r,c) board = 
     case r of 
      1 ->  Array.set c s (board.rowOne)
      2 -> Array.set c s (board.rowTwo)
      3 -> Array.set c s (board.rowThree)
      _ -> board

getCell : (Int, Int) -> String 
getCell (r,c) board = 
   case r of 
    1 -> case Array.get c sof 
           Nothing -> ""
           (Just v) -> v
    2 -> case Array.get c sof 
           Nothing -> ""
           (Just v) -> v
    3 -> case Array.get c sof 
           Nothing -> ""
           (Just v) -> v
    _ -> board
-- UPDATE

data Shape = Circle 
           | X 

type Msg = ClickedMe (Int Int)
         | Chose Shape
         | NPCChoice (Int,Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model,Cmd.none)
  case msg of 
   (Chose shape) -> ({model | playingAs = Just shape} Cmd.none)
   (ClickedMe t) -> case model.playingAs of 
                     Nothing -> (model,Cmd.none)
                     (Just s) -> ({model | myBoard = updateBoard s t model.myBoard}
                                 , generate NPCChoice uniformRowPicker)
   (NPCChoice t) -> case model.playingAs of 
                     Nothing  -> (model,Cmd.none)
                     (Just s) -> ({model | myBoard = updateBoard (notShape s) t model.myBoard} Cmd.none)


uniformRowPicker : Model -> Generator (Int,Int)
uniformRowPicker = 
      uniform (1,1) ([(1,1),(1,2),(1,3)
                     ,(2,1),(2,2),(2,3)
                     ,(3,1),(3,2),(3,3)
                     ]
                    )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Tic-Tac-Two version2 by Alec Rodriguez"]
    , chooseShape 
    , viewBoard model
    ]

chooseShape : Html Msg 
chooseShape = div 
              []
              [h2 [] [text "Tic-Tac-Two version2 by Alec Rodriguez"]
              ,button [onClick <| Chose Circle] [text "Play as Circle"] 
              ,button [onClick <| Chose X] [text "Play as X"]
              ]
              
viewBoard : Model -> Html Msg
viewBoard model =
  case model.playingAs of
   Nothing       -> text "Choose your shape before you can play."
   _             -> div 
                    boardAttributes 
                    [div[style "text-align" "center", onClick <| ClickedMe (1,1)][text <| getCell (1,1) model.myBoard]
                    ,div[style "text-align" "center", onClick <| ClickedMe (1,2)][text <| getCell (1,2) model.myBoard]
                    ,div[style "text-align" "center", onClick <| ClickedMe (1,3)][text <| getCell (1,3) model.myBoard]
                    ,div[style "text-align" "center", onClick <| ClickedMe (2,1)][text <| getCell (2,1) model.myBoard]
                    ,div[style "text-align" "center", onClick <| ClickedMe (2,2)][text <| getCell (2,2) model.myBoard]
                    ,div[style "text-align" "center", onClick <| ClickedMe (2,3)][text <| getCell (2,3) model.myBoard]
                    ,div[style "text-align" "center", onClick <| ClickedMe (3,1)][text <| getCell (3,1) model.myBoard]
                    ,div[style "text-align" "center", onClick <| ClickedMe (3,2)][text <| getCell (3,2) model.myBoard]
                    ,div[style "text-align" "center", onClick <| ClickedMe (3,3)][text <| getCell (3,3) model.myBoard]
                    ]


 
boardAttributes : List Attributes
boardAttributes =         
        [style "display", "grid"
        ,style "grid-template-columns" "repeat(3, 1fr)"
        ,style "gap" "10px"
        ] 
