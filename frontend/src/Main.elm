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
                 ,playingAs : Maybe Shape
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
      1 ->  {board | rowOne = Array.set c (showShape s) (board.rowOne)}
      2 -> {board | rowTwo = Array.set c (showShape s) (board.rowTwo)}
      3 -> {board | rowThree = Array.set c (showShape s) (board.rowThree)}
      _ -> board

showShape : Shape -> String 
showShape s = case s of 
               Circle -> "Circle"
               X      -> "X"

getCell : (Int, Int) -> Board -> String 
getCell (r,c) board = 
   case r of 
    1 -> case Array.get c board.rowOne of 
           Nothing -> ""
           (Just v) -> v
    2 -> case Array.get c board.rowTwo of 
           Nothing -> ""
           (Just v) -> v
    3 -> case Array.get c board.rowThree of 
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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
   (Chose shape) -> ({model | playingAs = Just shape}, Cmd.none)
   (ClickedMe t) -> case model.playingAs of 
                     Nothing -> (model,Cmd.none)
                     (Just s) -> ({model | myBoard = updateBoard s t model.myBoard}
                                 , generate NPCChoice (uniformRowPicker model.myBoard)
                                 )
   (NPCChoice t) -> case model.playingAs of 
                     Nothing  -> (model,Cmd.none)
                     (Just s) -> ({model | myBoard = updateBoard (notShape s) t model.myBoard}, Cmd.none)

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
   (Just _) -> div []
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
   _             -> div 
                    boardAttributes 
                    [div(cellAttributes ++ [onClick <| ClickedMe (1,1)])[text <| getCell (1,1) model.myBoard]
                    ,div(cellAttributes ++ [onClick <| ClickedMe (1,2)])[text <| getCell (1,2) model.myBoard]
                    ,div(cellAttributes ++ [onClick <| ClickedMe (1,3)])[text <| getCell (1,3) model.myBoard]
                    ,div(cellAttributes ++ [onClick <| ClickedMe (2,1)])[text <| getCell (2,1) model.myBoard]
                    ,div(cellAttributes ++ [onClick <| ClickedMe (2,2)])[text <| getCell (2,2) model.myBoard]
                    ,div(cellAttributes ++ [onClick <| ClickedMe (2,3)])[text <| getCell (2,3) model.myBoard]
                    ,div(cellAttributes ++ [onClick <| ClickedMe (3,1)])[text <| getCell (3,1) model.myBoard]
                    ,div(cellAttributes ++ [onClick <| ClickedMe (3,2)]) [text <| getCell (3,2) model.myBoard]
                    ,div(cellAttributes ++ [onClick <| ClickedMe (3,3)] )[text <| getCell (3,3) model.myBoard]
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