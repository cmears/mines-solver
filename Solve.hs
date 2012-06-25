import System.Process
import Control.Concurrent
import Codec.Picture
import Data.Function
import Data.List
import Control.Monad
import qualified Data.Map as M
import MinesImage
import System.IO
import Logic
import System.CPUTime
import TimeAccounting

solvestep account initBoard = do
  -- Get a screenshot of the window.
  accountAction account "screengrab" $ do
    accountAction account "dump" $ system "xdotool search --onlyvisible --name Mines | xargs xwd -id > xwddump"
    accountAction account "imagemagick" $ system "convert xwddump out.png"

  -- Read in the image.
  (ImageRGB8 img) <- liftM (either error id) (readImage "out.png")
  let tlc = findTopLeftCorner img
      cs = cellCoords img tlc
      conts = cellContents img cs (1,2)
      (cxs, cys, sz) = cs
      nxs = length cxs
      nys = length cys
  print (nxs, nys)

  board <- accountAction account "scraping" $ do
    let b = M.union (M.filter revealed initBoard) $
              M.fromList [ ((x,y), c) | x <- [0..nxs-1],
                                        y <- [0..nys-1],
                                        let c = cellIdentify img cs (x,y) ]
    forceBoard b
    return b

  forM_ [ (x,y) | y <- [0..nys-1], x <- [0..nxs-1] ] $ \(x,y) -> do
    putStr (showCell ((M.!) board (x,y)))
    when (x == nxs-1) (putStr "\n")

  g <- do
    p <- accountAction account "propagation" $ do
           propagate account board
    if null p
      then do
        putStrLn "RESORTING TO SEARCH"
        hFlush stdout
        accountAction account "guess testing" $ do
          g' <- findGuess account board (groupBy ((==) `on` fst) (possibleGuesses board)) 99999
          print g'
          return g'
      else return p
  
  shouldContinue <- accountAction account "clicking" $ do
    case g of
      [] -> print "sorry, couldn't figure anything out" >> return False
      gs -> do
             let bigcmd = "xdotool search --onlyvisible --name Mines " ++
                          concatMap ( \((x,y),b) ->
                                          let xpix = cxs !! x
                                              ypix = cys !! y
                                          in "mousemove --window %1 " ++ show xpix ++ " " ++ show ypix ++ " click " ++ (if b then "1" else "3") ++ " " ) gs
             print bigcmd
             system $ bigcmd
             return True

  when shouldContinue $ do
    -- Delay in case of flashing board
    threadDelay 300000
    solvestep account board

findGuess account board _ _ | M.size (M.filter revealed board) == 0 = return [((0,0), True)]
findGuess account board [] _ = return []
findGuess account board _ 0 = return []
findGuess account board (gs:gss) n = do
  r <- myFindM (testGuess account board) gs
  case r of
    Nothing -> do
      rest <- findGuess account board gss n
      return rest
    Just g -> do
      rest <- findGuess account board gss (n-1)
      return (g:rest)

myFindM :: (a -> IO Bool) -> [a] -> IO (Maybe a)
myFindM f [] = return Nothing
myFindM f (x:xs) = do
  r <- f x
  if r
    then return (Just x)
    else myFindM f xs

main = do
  account <- accountInit
  accountAction account "everything" $
    solvestep account M.empty
  accountSummary account

forceBoard :: M.Map (Int,Int) Cell -> IO ()
forceBoard board =
  forM_ (M.toList board) (\(_,c) -> return $! c)
