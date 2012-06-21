module Logic where

import           Data.List
import qualified Data.Map as M
import           Data.Ord
import           System.Exit
import           System.IO
import           System.Process
import           Text.Printf
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
possibleGuesses board = -- sortBy (comparing unrevealedNeighbours)
                        (filter ((/=0) . revealedNeighbours) guesses)
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


-- possibleGuesses :: Board -> [ ((Int,Int), Bool) ]
-- possibleGuesses board = concatMap (++"\n") items
--   where cells = M.filter (not . revealed) board
--         guesses = concatMap (\ ((x,y),Unknown) -> [ ((x,y),True), ((x,y),False) ]) (M.toList cells)
--         nguesses = length guesses
--         guessstrings = map (\ ((x,y),b) -> printf "%d,%d,%d" x y (if b then 1 else 0)) guesses
--         items = [ printf "nguesses = %d;" nguesses
--                 , "guesses = [| " ++ intercalate " | " guessstrings ++ " |];" ]

-- -- Returns True if the guess is *unsatisfiable*.
-- testGuess :: Account -> Board -> ((Int,Int), Bool) -> IO Bool
-- testGuess account board ((x,y),b) = do
--     withFile "out.dzn" WriteMode $ \h -> do
--       hPutStrLn h (boardToDzn board)
--       hPutStrLn h "nguesses = 1;"
--       hPutStrLn h (printf "guesses = [| %d,%d,%d |];" x y (fromEnum b))
--     r1 <- accountAction account "flattening" $
--             system "mzn2fzn minesweeper.mzn out.dzn 2>&1 | grep 'Model inconsistency'"
--     if r1 == ExitSuccess
--       then return True
--       else do
--         r <- accountAction account "fz" $
--                system "~/gecode/gecode-3.7.2/tools/flatzinc/fz -mode stat minesweeper.fzn | tee -a stats | head -n 1 | grep UNSAT"
--         return (r == ExitSuccess)

testGuess :: Account -> Board -> ((Int,Int), Bool) -> IO Bool
testGuess account board ((x,y),b) = do
    withFile "out.dzn" WriteMode $ \h -> do
      hPutStrLn h (boardToDzn board)
      hPutStrLn h "nguesses = 1;"
      hPutStrLn h (printf "guesses = [| %d,%d,%d |];" x y (fromEnum b))
    withFile "gecode-input" WriteMode $ \h -> do
      hPutStrLn h (show (fst (fst (M.findMax board)) + 1))
      hPutStrLn h (show (snd (fst (M.findMax board)) + 1))
      hPrintf h "%d %d %d\n" x y (fromEnum b)
      let clues = M.filter revealed board
          nclues = M.size clues
      mapM_ (\ ((xx,yy),cell) -> hPrintf h "%d %d %d\n" xx yy (revealedToN cell)) (M.toList clues)
    r <- accountAction account "gecode" $
               system "./a.out < gecode-input"
    return (r == ExitFailure 1)


