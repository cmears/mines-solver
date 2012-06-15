module MinesImage where

import Codec.Picture
import Control.Arrow ((&&&))
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Data.Word
import Debug.Trace

import Logic

type Img = Image PixelRGB8

type CellCoords = ([Int], [Int], Int)

findTopLeftCorner :: Img -> (Int,Int)
findTopLeftCorner img = trace (show (matchCoord,matchPix)) topLeftCoord
  where
    w = imageWidth img
    h = imageHeight img
    (cx,cy) = (w `div` 2, h `div` 2)
    diagCoords = reverse (zip [cx,cx-1..0] [cy,cy-1..0])
    pixAtCoord = pixelToTuple . uncurry (pixelAt img)
    diagPixels = map pixAtCoord diagCoords
    (match:_) = locateSubstring targetBlock diagPixels
    matchCoord = diagCoords !! (match+2)
    matchPix = pixAtCoord matchCoord
    abovePix = pixAtCoord (above matchCoord)
    leftPix = pixAtCoord (left matchCoord)
    direction = case (abovePix == topLeftEdgePixel,
                      leftPix == topLeftEdgePixel) of
                  (True, True) -> error "!"
                  (False, True) -> left
                  (True, False) -> above
                  (False, False) -> above
    topLeftCoord = last (takeWhile (pixAtIsEdge img) (iterate direction matchCoord))

cellCoords :: Img -> (Int,Int) -> CellCoords
cellCoords img topLeftCoord = (xcentres, ycentres, cellMinLength)
  where
    h = imageHeight img
    w = imageWidth img
    coords = iterate belowRight topLeftCoord
    afterEdge = dropWhile (pixAtIsEdge img) coords
    topLeftNonEdge = head afterEdge
    nextOne = belowRight topLeftNonEdge
    row = map (\c -> (c, pixAt img c)) (takeWhile ((<w) . fst) (iterate right nextOne))
    grow = map (head &&& length) . groupBy ((==) `on` snd) $ row
    cellMinLength = snd (head grow) - 2
    cellMaxLength = snd (head grow) + 3
    rowcells = filter (\ (_,l) -> cellMinLength <= l) (takeWhile ((<= cellMaxLength) . snd) grow)
    xcentres = map (\ (((x,_),_p), l) -> x + (l `div` 2)) rowcells

    col = map (\c -> (c, pixAt img c)) (takeWhile ((<h) . snd) (iterate below nextOne))
    gcol = map (head &&& length) . groupBy ((==) `on` snd) $ col
    colcells = filter (\ (_,l) -> cellMinLength <= l) (takeWhile ((<= cellMaxLength) . snd) gcol)
    ycentres = map (\ (((_,y),_p), l) -> y + (l `div` 2)) colcells
    
pixAt img = pixelToTuple . uncurry (pixelAt img)
pixAtIsEdge img c = pixAt img c == topLeftEdgePixel

above (x,y) = (x,y-1)
left (x,y) = (x-1,y)
right (x,y) = (x+1,y)
belowRight (x,y) = (x+1,y+1)
below (x,y) = (x,y+1)

pixelToTuple (PixelRGB8 r b g) = (r,b,g)

backgroundPixel = (220, 218, 213)
unknownPixel = backgroundPixel
topLeftEdgePixel = (147, 145, 142)
revealedPixel = (209,207,203)
targetBlock = replicate 2 backgroundPixel ++ replicate 2 topLeftEdgePixel

isBright (r,g,b) = r+g+b >= 500

locateSubstring :: Eq a => [a] -> [a] -> [Int]
locateSubstring needle haystack =
  map fst $ filter (\(i,s) -> needle `isPrefixOf` s) indexedTails
  where indexedTails = zip [0..] (tails haystack)


cellContents :: Img -> CellCoords -> (Int,Int) -> [ ((Word8,Word8,Word8), Int) ]
cellContents img (xcs, ycs, l) (x,y) = reverse . sortBy (comparing snd) . map (head &&& length) . group $ sortedPixels
  where (ccx, ccy) = (xcs !! x, ycs !! y)
        range = [-l `div` 2 .. l `div` 2]
        coords = [ (ccx + i, ccy + j) | i <- range, j <- range ]
        pixels = map (pixAt img) coords
        sortedPixels = sort pixels
        
colours :: [ ((Word8,Word8,Word8), Int) ]
colours = [ ((0,0,255), 1)
          , ((0,128,0), 2)
          , ((255,0,0), 3)
          , ((0,0,128), 4)
          , ((128,0,0), 5)
          , ((0,128,128), 6)
          , ((0,0,0),   7)
          , (backgroundPixel, -1)
          ]

cellIdentify :: Img -> CellCoords -> (Int,Int) -> Cell
cellIdentify img ccs coord = result
  where conts = cellContents img ccs coord
        mainColours = catMaybes (map (\p -> lookup (fst p) colours) conts)
        result = case mainColours of
                   (-1:3:7:_) -> Flagged
                   (-1:7:3:_) -> Flagged
                   (-1:_) -> Unknown
                   (n:_) -> Revealed n
                   _ -> Revealed 0
