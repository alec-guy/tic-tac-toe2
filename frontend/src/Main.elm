module Main exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Random as R exposing (..)
import Json.Decode as D exposing (succeed)


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
                 ,playingAs : Maybe Shape
                 ,winner : Maybe Shape 
                 }



type alias Board =
  { rowOne   : Array String
  , rowTwo   : Array String
  , rowThree : Array String
  }


init : () -> (Model, Cmd Msg)
init _ = (initialModel, Cmd.none)

initialModel : Model 
initialModel = {myBoard = initialBoard, playingAs = Nothing, winner = Nothing}
initialBoard = 
           {rowOne =   fromList ["","",""]
           ,rowTwo =   fromList ["","",""]
           ,rowThree = fromList ["","",""]
           }
updateBoard : Shape -> (Int,Int) -> Board -> Board 
updateBoard s (r,c) board = 
     case r of 
      1 ->  {board | rowOne = Array.set (c - 1) (showShape s) (board.rowOne)}
      2 -> {board | rowTwo = Array.set (c - 1) (showShape s) (board.rowTwo)}
      3 -> {board | rowThree = Array.set (c - 1) (showShape s) (board.rowThree)}
      _ -> board

showShape : Shape -> String 
showShape s = case s of 
               Circle -> "O"
               X      -> "X"

getCell : (Int, Int) -> Board -> String 
getCell (r,c) board = 
   case r of 
    1 -> case Array.get (c - 1) board.rowOne of 
           Nothing -> ""
           (Just v) -> v
    2 -> case Array.get (c - 1) board.rowTwo of 
           Nothing -> ""
           (Just v) -> v
    3 -> case Array.get (c - 1) board.rowThree of 
           Nothing -> ""
           (Just v) -> v
    _ -> ""
-- UPDATE

type Shape = Circle | X 
notShape : Shape -> Shape 
notShape s = case s of 
              Circle -> X 
              X      -> Circle 
type Msg = ClickedMe (Int,Int)
         | Chose Shape
         | NPCChoice (Int,Int)
         | NewGame 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
   (Chose shape) -> ({initialModel | playingAs = Just shape}, Cmd.none)
   (ClickedMe t) -> case model.playingAs of 
                     Nothing -> (model,Cmd.none)
                     (Just s) -> let newBoard = updateBoard s t model.myBoard 
                                 in ({model | myBoard = newBoard}
                                    ,generate NPCChoice (uniformRowPicker newBoard)
                                    )
   (NPCChoice t) -> case model.playingAs of 
                     Nothing  -> (model,Cmd.none)
                     (Just s) -> ({model | myBoard = updateBoard (notShape s) t model.myBoard}, Cmd.none)
   (NewGame) -> (initialModel , Cmd.none)
                       
---- 
-- EVAL Board
evalRow : List String -> Maybe Shape 
evalRow row = 
     case row of 
      ["X" ,"X" ,"X"]   -> Just X 
      ["O", "O", "O"] -> Just Circle
      _               -> Nothing 
      
evalRows : Board -> Maybe Shape 
evalRows board = 
         
     let rowOneEval   = evalRow (Array.toList board.rowOne) 
         rowTwoEval   = evalRow (Array.toList board.rowTwo)
         rowThreeEval = evalRow (Array.toList board.rowThree) 
     in asum[rowOneEval, rowTwoEval,rowThreeEval]
       
asum : List (Maybe a) -> Maybe a 
asum l = case l of 
      [] -> Nothing
      (x :: xs) -> case x of 
                    Nothing  -> asum xs
                    (Just y) -> Just y
     
evalColumns : Board -> Maybe Shape 
evalColumns board = 
    case (Array.toList board.rowOne,Array.toList board.rowTwo,Array.toList board.rowThree) of 
    (["X",_,_],["X",_,_],["X",_,_]) -> Just X 
    ([_,"X",_],[_,"X",_],[_,"X",_]) -> Just X 
    ([_,_,"X"],[_,_,"X"],[_,_,"X"]) -> Just X 
    (["O",_,_],["O",_,_],["O",_,_]) -> Just Circle
    ([_,"O",_],[_,"O",_],[_,"O",_]) -> Just Circle
    ([_,_,"O"],[_,_,"O"],[_,_,"O"]) -> Just Circle 
    _                               -> Nothing 
evalDiag : Board -> Maybe Shape 
evalDiag board = 
  case (Array.toList board.rowOne,Array.toList board.rowTwo,Array.toList board.rowThree) of 
   (["X",_,_],[_,"X",_],[_,_,"X"]) -> Just X 
   ([_,_,"X"],[_,"X",_],["X",_,_]) -> Just X 
   (["O",_,_],[_,"O",_],[_,_,"O"]) -> Just Circle
   ([_,_,"O"],[_,"O",_],["O",_,_]) -> Just Circle 
   _                               -> Nothing 
evalBoard : Board -> Maybe Shape 
evalBoard board = asum [evalRows board, evalColumns board, evalDiag board]

-------------------------------------


    
getAvailableSpots : Board -> List (Int,Int)
getAvailableSpots board = 
        let rowOneSpots  = loopingFunction 1 0 board.rowOne 
            rowTwoSpots  = loopingFunction 2 0 board.rowTwo 
            rowThreeSpots = loopingFunction 3 0 board.rowThree 
        in rowOneSpots ++ rowTwoSpots ++ rowThreeSpots
loopingFunction : Int -> Int -> Array String -> List (Int,Int)
loopingFunction i acc array = 
       case Array.toList array of 
        [] -> [] 
        (s :: ss) -> case s of 
                      "" -> (i, 1 + acc) :: (loopingFunction i (acc + 1) (Array.fromList ss))
                      _  -> loopingFunction i (acc + 1) (Array.fromList ss)
uniformRowPicker : Board -> Generator (Int,Int)
uniformRowPicker board = 
      uniform (1,1) (getAvailableSpots board)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model.playingAs of 
   Nothing -> chooseShape 
   _       ->  div []
               [ h2 [] [ text "Tic-Tac-Two version2 by Alec Rodriguez"]
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
   _             -> case evalBoard model.myBoard of 
                     Nothing -> 
                         div 
                         boardAttributes 
                         [div(cellAttributes ++ [onClick <| ClickedMe (1,1)])[text <| getCell (1,1) model.myBoard]
                         ,div(cellAttributes ++ [onClick <| ClickedMe (1,2)])[text <| getCell (1,2) model.myBoard]
                         ,div(cellAttributes ++ [onClick <| ClickedMe (1,3)])[text <| getCell (1,3) model.myBoard]
                         ,div(cellAttributes ++ [onClick <| ClickedMe (2,1)])[text <| getCell (2,1) model.myBoard]
                         ,div(cellAttributes ++ [onClick <| ClickedMe (2,2)])[text <| getCell (2,2) model.myBoard]
                         ,div(cellAttributes ++ [onClick <| ClickedMe (2,3)])[text <| getCell (2,3) model.myBoard]
                         ,div(cellAttributes ++ [onClick <| ClickedMe (3,1)])[text <| getCell (3,1) model.myBoard]
                         ,div(cellAttributes ++ [onClick <| ClickedMe (3,2)])[text <| getCell (3,2) model.myBoard]
                         ,div(cellAttributes ++ [onClick <| ClickedMe (3,3)])[text <| getCell (3,3) model.myBoard]
                         ]
                     (Just shape) -> 
                           div 
                           [] 
                           [h1 
                            [] 
                            [div [] [text <| if shape == Circle then "Circle wins!" else "X" ++ " wins!"]
                            ,div [] [button [onClick NewGame] [text "Play new game"]]
                            ]
                          ,div 
                           boardAttributes 
                           [div(cellAttributes)[text <| getCell (1,1) model.myBoard]
                           ,div(cellAttributes)[text <| getCell (1,2) model.myBoard]
                           ,div(cellAttributes)[text <| getCell (1,3) model.myBoard]
                           ,div(cellAttributes)[text <| getCell (2,1) model.myBoard]
                           ,div(cellAttributes )[text <| getCell (2,2) model.myBoard]
                           ,div(cellAttributes)[text <| getCell (2,3) model.myBoard]
                           ,div(cellAttributes)[text <| getCell (3,1) model.myBoard]
                           ,div(cellAttributes)[text <| getCell (3,2) model.myBoard]
                           ,div(cellAttributes)[text <| getCell (3,3) model.myBoard]
                           ]

                           ]


 
 -----------------
 --- AI MADE WHAT IS BELOW ME 
 
boardAttributes : List (Html.Attribute Msg )
boardAttributes =         
        [style "display" "grid"
        ,style "grid-template-columns" "repeat(3, 1fr)"
        ,style "gap" "10px"
        ,style "width" "300px"
        ,style "margin" "0 auto"
        ,style "border" "2px solid black"
        ,style "padding" "10px"
        ] 

cellAttributes : List (Html.Attribute Msg)
cellAttributes = 
        [style "border" "1px solid black"
        ,style "text-align" "center"
        ,style "padding" "20px"
        ,style "background-color" "#f0f0f0"
        ,style "cursor" "pointer"
        ]