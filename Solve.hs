import System.Process
import Codec.Picture
import Control.Monad
import qualified Data.Map as M
import MinesImage
import System.IO
import Logic

main = do
  -- Get a screenshot of the window.
  system "xwd -name Mines | xwdtopnm | convert - out.png"

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

  let board = M.fromList [ ((x,y), c) | x <- [0..nxs-1], y <- [0..nys-1], let c = cellIdentify img cs (x,y) ]

  forM_ [ (x,y) | y <- [0..nys-1], x <- [0..nxs-1] ] $ \(x,y) -> do
    putStr (showCell ((M.!) board (x,y)))
    when (x == nxs-1) (putStr "\n")

  let xpix = cxs !! 2
      ypix = cys !! 2
      cmd = "xdotool search --onlyvisible Mines mousemove --window %1 " ++ show xpix ++ " " ++ show ypix ++ " sleep 0.1 click 1"
  print cmd
  system $ cmd

--  putStrLn (boardToDzn board)

--  print (possibleGuesses board)

  r <- testGuess board ((2,0),False)
  print r

--  withFile "out.dzn" WriteMode (\h -> hPutStrLn h (boardToDzn board))


  return ()
