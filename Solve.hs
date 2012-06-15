import System.Process
import Codec.Picture
import Control.Monad
import qualified Data.Map as M
import MinesImage
import System.IO
import Logic

solvestep initBoard = do
  -- Get a screenshot of the window.
  system "xdotool search --onlyvisible --name Mines | xargs xwd -id | xwdtopnm | convert - out.png"

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

  let board = M.union (M.filter revealed initBoard) $ M.fromList [ ((x,y), c) | x <- [0..nxs-1], y <- [0..nys-1], let c = cellIdentify img cs (x,y) ]

  forM_ [ (x,y) | y <- [0..nys-1], x <- [0..nxs-1] ] $ \(x,y) -> do
    putStr (showCell ((M.!) board (x,y)))
    when (x == nxs-1) (putStr "\n")

  -- let xpix = cxs !! 2
  --     ypix = cys !! 2
  --     cmd = "xdotool search --onlyvisible --name Mines mousemove --window %1 " ++ show xpix ++ " " ++ show ypix ++ " sleep 0.1 click 1"
  -- print cmd
  -- system $ cmd

--  putStrLn (boardToDzn board)

--  print (possibleGuesses board)

  g <- findGuess board (possibleGuesses board)
  print g

  case g of
    [] -> print "sorry, couldn't figure anything out"
    gs -> do
           forM_ gs $ \ ((x,y),b) -> do
             let xpix = cxs !! x
                 ypix = cys !! y
                 cmd = "xdotool search --onlyvisible --name Mines mousemove --window %1 " ++ show xpix ++ " " ++ show ypix ++ " sleep 0.1 click " ++ (if b then "1" else "3")
             print cmd
             system $ cmd
           solvestep board

--  withFile "out.dzn" WriteMode (\h -> hPutStrLn h (boardToDzn board))


findGuess board [] = return []
findGuess board (g:gs) = do
  putStrLn $ "testing " ++ show g
  r <- testGuess board g
  rest <- findGuess board gs
  if r
    then return (g:rest)
    else return rest
   

main = solvestep M.empty
