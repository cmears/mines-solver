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
--    accountAction account "dumptopnm" $ system "xwdtopnm < xwddump > dump.ppm"
    accountAction account "imagemagick" $ system "convert xwddump out.png"

  -- Read in the image.
  (ImageRGB8 img) <- liftM (either error id) (readImage "out.png")

  -- case img of
  --   ImageY8 _ -> putStrLn "y8"
  --   ImageYA8 _ -> putStrLn "ya8"
  --   ImageRGB8 _ -> putStrLn "rgb8"
  --   ImageRGBA8 _ -> putStrLn "rgba8"
  --   ImageYCbCr8 _ -> putStrLn "ycbcr8"

  let (PixelRGB8 r b g) = pixelAt img 10 10
  print (r,g,b)

  let tlc = findTopLeftCorner img
  print tlc
  let cs = cellCoords img tlc
  print cs

  let conts = cellContents img cs (1,2)
  print conts

  let (cxs, cys, sz) = cs
  let nxs = length cxs
  let nys = length cys
  print (nxs, nys)

  board <- accountAction account "scraping" $ do
    let b = M.union (M.filter revealed initBoard) $ M.fromList [ ((x,y), c) | x <- [0..nxs-1], y <- [0..nys-1], let c = cellIdentify img cs (x,y) ]
    forceBoard b
    return b

  forM_ [ (x,y) | y <- [0..nys-1], x <- [0..nxs-1] ] $ \(x,y) -> do
    putStr (showCell ((M.!) board (x,y)))
    when (x == nxs-1) (putStr "\n")

--  getLine
--  getLine

  -- let xpix = cxs !! 2
  --     ypix = cys !! 2
  --     cmd = "xdotool search --onlyvisible --name Mines mousemove --window %1 " ++ show xpix ++ " " ++ show ypix ++ " sleep 0.1 click 1"
  -- print cmd
  -- system $ cmd

--  putStrLn (boardToDzn board)

--  print (possibleGuesses board)

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
             -- forM_ gs $ \ ((x,y),b) -> do
             --   let xpix = cxs !! x
             --       ypix = cys !! y
             --       cmd = "xdotool search --onlyvisible --name Mines mousemove --window %1 " ++ show xpix ++ " " ++ show ypix ++ " sleep 0.01 click " ++ (if b then "1" else "3")
             print bigcmd
             system $ bigcmd
             return True

  when shouldContinue $ do
    -- Delay in case of flashing board
    threadDelay 300000
    solvestep account board


--  withFile "out.dzn" WriteMode (\h -> hPutStrLn h (boardToDzn board))


findGuess account board _ _ | M.size (M.filter revealed board) == 0 = return [((0,0), True)]
findGuess account board [] _ = return []
findGuess account board _ 0 = return []
findGuess account board (gs:gss) n = do
  r <- myFindM (testGuess account board) gs
--  putStrLn $ "testing " ++ show g
--  r <- testGuess board g
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
