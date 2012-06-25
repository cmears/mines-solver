module Logic where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Ord
import System.Exit
import System.IO
import System.Process
import Text.Printf
import TimeAccounting

data Cell = Unknown
          | Revealed Int
          | Flagged
  deriving Show
showCell Unknown = "."
showCell Flagged = "#"
showCell (Revealed 0) = "_"
showCell (Revealed n) = show n

revealed Unknown = False
revealed (Revealed _) = True
revealed Flagged = True

revealedToN Unknown = error "revealedToN"
revealedToN Flagged = (-1)
revealedToN (Revealed n) = n

type Board = M.Map (Int,Int) Cell

boardToDzn :: Board -> String
boardToDzn board = concatMap (++"\n") items
  where clues = M.filter revealed board
        nclues = M.size clues
        cluestrings = map (\ ((x,y),cell) -> printf "%d,%d,%d" x y (revealedToN cell)) (M.toList clues)
        items = [ printf "width = %d;" (fst (fst (M.findMax board)) + 1)
                , printf "height = %d;" (snd (fst (M.findMax board)) + 1)
                , printf "nclues = %d;" nclues
                , "clues = [| " ++ intercalate " | " cluestrings ++ " |];"
                ]

possibleGuesses :: Board -> [ ((Int,Int), Bool) ]
possibleGuesses board = (filter ((/=0) . revealedNeighbours) guesses)
  where cells = M.filter (not . revealed) board
        guesses = concatMap (\ ((x,y),Unknown) -> [ ((x,y),True), ((x,y),False) ]) (M.toList cells)
        revealedNeighbours ((x,y),b) = length (filter g [(x+i,y+j) | i <- [-1,0,1], j <- [-1,0,1]]) 
        unrevealedNeighbours ((x,y),b) = length (filter f [(x+i,y+j) | i <- [-1,0,1], j <- [-1,0,1]])
        f (x,y) = case M.lookup (x,y) board of
                    Nothing -> False
                    Just Unknown -> True
                    _ -> False
        g (x,y) = case M.lookup (x,y) board of
                    Nothing -> False
                    Just (Revealed _) -> True
                    _ -> False

testGuess :: Account -> Board -> ((Int,Int), Bool) -> IO Bool
testGuess account board ((x,y),b) = do
    withFile "gecode-input" WriteMode $ \h -> do
      hPutStrLn h "0"
      hPutStrLn h (show (fst (fst (M.findMax board)) + 1))
      hPutStrLn h (show (snd (fst (M.findMax board)) + 1))
      hPrintf h "%d %d %d\n" x y (fromEnum b)
      let clues = M.filter revealed board
          nclues = M.size clues
      mapM_ (\ ((xx,yy),cell) -> hPrintf h "%d %d %d\n" xx yy (revealedToN cell)) (M.toList clues)
    r <- accountAction account "gecode" $
               system "./a.out < gecode-input"
    return (r == ExitFailure 1)

propagate :: Account -> Board -> IO [ ((Int,Int), Bool) ]
propagate account board = do
    withFile "gecode-prop-input" WriteMode $ \h -> do
      hPutStrLn h "1"
      hPutStrLn h (show (fst (fst (M.findMax board)) + 1))
      hPutStrLn h (show (snd (fst (M.findMax board)) + 1))
      let clues = M.filter revealed board
          nclues = M.size clues
      mapM_ (\ ((xx,yy),cell) -> hPrintf h "%d %d %d\n" xx yy (revealedToN cell)) (M.toList clues)
    r <- accountAction account "gecode-prop" $
         system "./a.out < gecode-prop-input > gecode-prop-output"
    c <- liftM lines (readFile "gecode-prop-output")
    when ((head c) == "failed") (error "propagation failed!")
    let output = map specialRead (tail c)
        usefulOutput = filter ((/= -1) . snd) output
        novelOutput = filter ( \((x,y),_) -> isUnknown (x,y) board ) usefulOutput
    return (map (\((x,y),b) -> ((x,y), not (toEnum b))) novelOutput)

specialRead :: String -> ((Int,Int),Int)
specialRead s =
    let [a,b,c] = map read $ words s
    in ((a,b),c)

isUnknown (x,y) board =
  case M.lookup (x,y) board of
    Nothing -> True
    Just Unknown -> True
    _ -> False
