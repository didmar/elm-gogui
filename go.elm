module Go(main) where

import String exposing (left, dropLeft, fromChar, lines, trim, cons, words, toInt)
import Keyboard
import List exposing (filter, filterMap, indexedMap, concatMap, map, head, foldr, isEmpty)
import Array exposing (Array, repeat, get, set, empty, toIndexedList)
import Text exposing (monospace, color, fromString)
import Window
import Signal exposing (Address, foldp, merge, sampleOn, constant)
import Signal.Extra exposing (zip, zip3, zip4, (<~), (~))
import Time exposing (second, every, fps)
import Graphics.Collage exposing (collage, move, filled, circle, rect, toForm, text, Form, solid, outlined, square, alpha)
import Graphics.Collage as Collage
import Graphics.Element exposing (Element, leftAligned, show)
import Color exposing (white, black, red, rgb, Color)
import Maybe exposing (withDefault, map, andThen, Maybe(..))
import Mouse exposing (clicks, position)
import Debug exposing (log, crash)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, (:=), array, string, int, map, object4, succeed)
import Http
import Http exposing (Error(BadResponse))
import Html exposing (Html, Attribute, div, fromElement, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Task exposing (Task, andThen, fail, succeed, onError)
import Char exposing (toCode, fromCode)

-- [ Game state ]

type Player = White | Black
type State  = Stone Player | Empty | EmptyHoshi
type alias Coords = (Int, Int)
type alias Row = Array State
type alias Board = Array (Array State)

playerToStr : Player -> String
playerToStr p = case p of
  White -> "white"
  Black -> "black"

playerFromStr : String -> Player
playerFromStr p = case p of
  "white" -> White
  "black" -> Black
  _ -> Debug.crash "invalid format in playerFromStr"

-- 0 -> A, 1 -> B, ... but skips I !!
toLetterCoord : Int -> Char
toLetterCoord x = fromCode (x + (toCode 'A') + (if x >= 8 then 1 else 0))

fromLetterCoord : Char -> Int
fromLetterCoord c =
  let x = (toCode c) - (toCode 'A')
  in x - (if x >= 9 then 1 else 0)

-- (0,0) -> "A1", (0,1) -> "B1", ...
toStringCoords : Coords -> String
toStringCoords (x,y) =
   cons (toLetterCoord y) (toString (x + 1))

-- "A1" -> (0,0), "B1" -> (0,1) ...
fromStringCoords : String -> Coords
fromStringCoords s =
  let f x y = case toInt (String.fromList y) of
      Ok yy -> (yy - 1, fromLetterCoord x)
      Err err -> Debug.crash "invalid format in fromStringCoords"
  in case String.toList s of
    x :: y -> Debug.log "fromStringCoords" (f x y)
    _ -> Debug.crash "invalid format in fromStringCoords"

fromStringCoordsOrPass : String -> Maybe Coords
fromStringCoordsOrPass pos = case pos of
  "PASS" -> Nothing
  x -> Just (fromStringCoords pos)

defaultBoardSize = 19

initBoard = Array.repeat defaultBoardSize (Array.repeat defaultBoardSize Empty)

type alias BoardSize = Int
type alias LastMove = Maybe Coords
type alias Captures = (Int, Int)
type GameState = GameState { board: Board, size: BoardSize, nextPlayer: Player, lastMove: LastMove, captures: Captures }

initGame = GameState { board = initBoard, size = defaultBoardSize, nextPlayer = Black, lastMove = Nothing, captures = (0,0) }

opponentOf : Player -> Player
opponentOf p = case p of
  White -> Black
  Black -> White

-- [ Inputs ]

type UserInput = NoInput | Played Coords | Passed | NewGame BoardSize

type alias WinDims    = (Int, Int)

boardDimsSgn : Signal WinDims
boardDimsSgn =
  let (num, den) = (3, 4) -- ratio of window height that the board display will take
  in Signal.map (\(w,h) -> (h * num // den, h * num // den)) Window.dimensions

boardSizeSgn = Signal.map (\(GameState gs) -> gs.size) gameStateAdr.signal

clickSgn : Signal UserInput
clickSgn =
  let f wh boardSize xy = withDefault NoInput (Maybe.map (\coords -> Played coords) (mousePosToCoords wh boardSize xy))
  in sampleOn Mouse.clicks (Signal.map3 f boardDimsSgn boardSizeSgn Mouse.position)

-- Mailbox for receiving inputs from buttons
otherUserInputAdr : Signal.Mailbox UserInput
otherUserInputAdr = Signal.mailbox NoInput

type alias Selection = Maybe Coords

quietSelectedSgn : Signal Selection
quietSelectedSgn =
  sampleOn Mouse.position (Signal.map3 mousePosToCoords boardDimsSgn boardSizeSgn Mouse.position)

verboseSelectedSgn : Signal Selection
verboseSelectedSgn =
  let f x = case x of
    Just xy -> "selected " ++ toStringCoords xy
    Nothing -> "selected nothing "
  in (\x -> log (f x) x) <~ quietSelectedSgn

-- selectedSgn = verboseSelectedSgn
selectedSgn = quietSelectedSgn

quietUserInput : Signal UserInput
quietUserInput = merge clickSgn otherUserInputAdr.signal

verboseUserInput : Signal UserInput
verboseUserInput =
  let f ui = case ui of
    NoInput -> "NoInput"
    Played (x,y) -> "Played " ++ toString x ++ "," ++ toString y
    Passed -> "Passed"
    NewGame boardSize -> "NewGame " ++ toString boardSize
  in (\x -> log (f x) x) <~ quietUserInput

-- userInput = quietUserInput
userInput = verboseUserInput

-- [ Graphics ]

woodRGB = rgb 180 150 160

nbSpacings : BoardSize -> Float
nbSpacings boardSize = (toFloat boardSize) + 1.0

hoshisCoords : List Coords
hoshisCoords  = [(9,9)] ++ concatMap (\x -> List.map (\y -> (x,y)) [4, 14]) [4, 14]

hoshiDiameter : WinDims -> BoardSize -> Float
hoshiDiameter (w,h) boardSize = toFloat (min w h) / (nbSpacings boardSize) / 4.0
hoshiRadius (w,h) boardSize = hoshiDiameter (w,h) boardSize / 2.0

stoneDiameter : WinDims -> BoardSize -> Float
stoneDiameter (w,h) boardSize = toFloat (min w h) / nbSpacings boardSize
stoneRadius (w,h) boardSize = stoneDiameter (w,h) boardSize / 2.0

koMarkSize : WinDims -> BoardSize -> Float
koMarkSize (w,h) boardSize = toFloat (min w h) / (nbSpacings boardSize) / 2.0

coordsToScreenPos : WinDims -> BoardSize -> Coords -> (Float, Float)
coordsToScreenPos (w,h) boardSize (x,y) =
  let d = stoneDiameter (w,h) boardSize
  in ( (toFloat (x+1)) * d - ((toFloat w) / 2.0),
       (toFloat (y+1)) * d - ((toFloat h) / 2.0) )

mousePosToCoords : WinDims -> BoardSize -> (Int, Int) -> Maybe Coords
mousePosToCoords (w,h) boardSize (sx,sy) =
  let d = stoneDiameter (w,h) boardSize
      x = round ((toFloat sx) / d - 1.0)
      y = round ((toFloat (h - sy)) / d - 1.0)
  in if x >= 0 && x < boardSize && y >= 0 && y < boardSize
     then Just (x,y)
     else Nothing

showCaptures : Captures -> String
showCaptures (bc, wc) =
  "Black captured " ++ toString bc ++ ", White captured " ++ toString wc

playerColor : Player -> Color
playerColor p = case p of
  White -> white
  Black -> black

displayStone : WinDims -> BoardSize -> Coords -> Float -> Player -> Form
displayStone wh boardSize xy alph p =
  circle (stoneRadius wh boardSize) |> filled (playerColor p) |> alpha alph |>  move (coordsToScreenPos wh boardSize xy)

-- displayKoMark : WinDims -> BoardSize -> Coords -> Form
-- displayKoMark wh boardSize xy =
--  square (koMarkSize wh boardSize) |> outlined (solid black) |>  move (coordsToScreenPos wh boardSize xy)

displayHoshi : WinDims -> BoardSize -> Coords -> Form
displayHoshi wh boardSize xy =
  circle (hoshiRadius wh boardSize) |> filled black |>  move (coordsToScreenPos wh boardSize xy)

displayMove : WinDims -> BoardSize -> Coords -> Float -> State -> Maybe Form
displayMove wh boardSize xy alph state =
  case state of
    Stone p    -> Just (displayStone wh boardSize xy alph p)
    EmptyHoshi -> Just (displayHoshi wh boardSize xy)
    Empty      -> Nothing

displayRow : WinDims -> BoardSize -> Board -> Int -> List Form
displayRow wh boardSize rows x =
  let row = Array.toList (withDefault empty (get x rows))
      f y mv = displayMove wh boardSize (x,y) 1.0 mv
  in filterMap identity (indexedMap f row)

displayBoard : WinDims -> BoardSize -> Board -> List Form
displayBoard wh boardSize rows =
  concatMap (\x -> displayRow wh boardSize rows x) [0..boardSize-1]

displayGrid : WinDims -> BoardSize -> List Form
displayGrid wh boardSize =
  let d = stoneDiameter wh boardSize
      f x y = let (sx, sy) = coordsToScreenPos wh boardSize (x,y)
              in square d |> outlined (solid black) |> (move (sx+0.5*d, sy+0.5*d))
  in concatMap (\x -> List.map (\y -> f x y) [0..boardSize-2]) [0..boardSize-2]

display : (WinDims, UserInput, Selection) -> GameState -> Element
display ((w,h), ui, select) (GameState gs) =
  let bgElem = rect (toFloat w) (toFloat h) |> filled woodRGB
      gridElem = displayGrid (w,h) gs.size
      boardElem = displayBoard (w,h) gs.size gs.board
      selectElem = case select of
        Just (x,y) -> case displayMove (w,h) gs.size (x,y) 0.5 (Stone gs.nextPlayer) of
                            Just form -> [form]
                            Nothing -> []
        Nothing -> []
  in collage w h (bgElem::(gridElem ++ boardElem ++ selectElem))

-- [ Webservice ]

serverURL = "http://0.0.0.0:5000/"

gameStateAdr : Signal.Mailbox GameState
gameStateAdr = Signal.mailbox initGame

showBoardDecoder : Decoder (Board, BoardSize, Captures)
showBoardDecoder =
    Json.Decode.map (\(b,s,bc,wc) -> (b, s, (bc, wc)))
                    (object4 (,,,) ("board" := boardDecoder) ("boardSize" := int) ("blackCaptures" := int) ("whiteCaptures" := int))

boardDecoder : Decoder Board
boardDecoder = array (array stateDecoder)

stateDecoder : Decoder State
stateDecoder =
  let f x = case x of
    "O" -> Stone White
    "X" -> Stone Black
    "." -> Empty
    "+" -> EmptyHoshi
    _   -> Debug.crash ("invalid pattern while decoding gnugo board: "++x)
  in Json.Decode.map f string

getBoard : Task Http.Error (Board, BoardSize, Captures)
getBoard = Http.get showBoardDecoder (serverURL ++ "showboard")

getLastMoveAndNextPlayer : Task Http.Error (LastMove, Player)
getLastMoveAndNextPlayer =
  let g err = case err of
        BadResponse 400 s -> Task.succeed ""
        _ -> fail err
      f s = case words s of
        col :: pos :: lst -> (fromStringCoordsOrPass pos, opponentOf (playerFromStr col))
        _ -> (Nothing, Black)
  in Task.map f (onError (Http.getString (serverURL ++ "last_move")) g)

mergeInfos : (Board, BoardSize, Captures) -> (LastMove, Player) -> GameState
mergeInfos (b,s,c) (l,p) = GameState {board=b, size=s, nextPlayer=p, lastMove=l, captures=c}

updateGameState : Task Http.Error ()
updateGameState =
  (Task.map2 mergeInfos getBoard getLastMoveAndNextPlayer)
  `Task.andThen` (\board -> Signal.send gameStateAdr.address board)

sendCommand : (UserInput, GameState) -> Task Http.Error String
sendCommand (ui, GameState gs) =
   let do url = Http.getString (Debug.log "calling" (serverURL ++ url))
       color = playerToStr gs.nextPlayer
   in case ui of
     Played xy    -> do ("play/" ++ color ++ "/" ++ toStringCoords xy)
     Passed       -> do ("play/" ++ color ++ "/pass")
     NewGame size -> do ("boardsize/" ++ toString size)
     _            -> Task.succeed "dummy"

-- Sends a command and update the game state
port gnugo : Signal (Task Http.Error ())
port gnugo =
  Signal.map (\input -> sendCommand input `Task.andThen` (\x -> updateGameState))
             (sampleOn userInput (Signal.Extra.zip userInput gameStateAdr.signal))

-- [ Main ]

boardStyle : WinDims -> Attribute
boardStyle (w,h) =
  style
    [ ("padding", "0px")
    , ("margin", "0px")
    , ("width", toString w ++ "px")
    , ("height", toString h ++ "px")
    , ("overflow", "hidden")
    , ("position", "relative")
    ]

view : Address UserInput -> (WinDims, UserInput, Selection, GameState) -> Html
view address (wh, ui, select, GameState gs) =
  div []
    [ div [ boardStyle wh ] [ fromElement (display (wh, ui, select) (GameState gs)) ]
    , div [] [ Html.text (showCaptures gs.captures) ]
    , button [ onClick address Passed ] [ Html.text "Pass" ]
    , button [ onClick address (NewGame gs.size) ] [ Html.text "New Game" ]
    ]

main : Signal Html
main = (\x -> view otherUserInputAdr.address x) <~ (Signal.Extra.zip4 boardDimsSgn userInput selectedSgn gameStateAdr.signal)
