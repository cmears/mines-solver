module Logic where

import           Data.List
import qualified Data.Map as M
import           System.Exit
import           System.IO
import           System.Process
import           Text.Printf

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
possibleGuesses board = guesses
  where cells = M.filter (not . revealed) board
        guesses = concatMap (\ ((x,y),Unknown) -> [ ((x,y),True), ((x,y),False) ]) (M.toList cells)

-- possibleGuesses :: Board -> [ ((Int,Int), Bool) ]
-- possibleGuesses board = concatMap (++"\n") items
--   where cells = M.filter (not . revealed) board
--         guesses = concatMap (\ ((x,y),Unknown) -> [ ((x,y),True), ((x,y),False) ]) (M.toList cells)
--         nguesses = length guesses
--         guessstrings = map (\ ((x,y),b) -> printf "%d,%d,%d" x y (if b then 1 else 0)) guesses
--         items = [ printf "nguesses = %d;" nguesses
--                 , "guesses = [| " ++ intercalate " | " guessstrings ++ " |];" ]

-- Returns True if the guess is satisfiable.
testGuess :: Board -> ((Int,Int), Bool) -> IO Bool
testGuess board ((x,y),b) = do
    withFile "out.dzn" WriteMode $ \h -> do
      hPutStrLn h (boardToDzn board)
      hPutStrLn h "nguesses = 1;"
      hPutStrLn h (printf "guesses = [| %d,%d,%d |];" x y (fromEnum b))
    system "mzn2fzn minesweeper.mzn out.dzn"
    r <- system "~/gecode/gecode-3.7.2/tools/flatzinc/fz minesweeper.fzn"
    return (r == ExitSuccess)


