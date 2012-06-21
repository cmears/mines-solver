module TimeAccounting where

import Data.IORef
import qualified Data.Map as M
import Foreign.C.Types
import System.CPUTime
import Control.Monad
import System.Posix.Process
import System.Posix.Types
import Text.Printf

data Times = Times {
      cpuTime :: Double -- as reported by getCPUTime, but converted into milliseconds
    , processTimes :: ProcessTimes -- as reported by getProcessTimes
    }

showTimes :: Times -> String
showTimes (Times { cpuTime = c, processTimes = pt }) =
    printf "CPU: %.1fms, elapsed: %.1fms, user: %.1fms, sys: %.1fms, childuser: %.1fms, childsys: %.1fms"
           c e u s cu cs
  where
    e =  ((fromIntegral . clockTickToInteger) (elapsedTime pt)     / clockTicksPerMillisec) :: Double
    u =  ((fromIntegral . clockTickToInteger) (userTime pt)        / clockTicksPerMillisec) :: Double
    s =  ((fromIntegral . clockTickToInteger) (systemTime pt)      / clockTicksPerMillisec) :: Double
    cu = ((fromIntegral . clockTickToInteger) (childUserTime pt)   / clockTicksPerMillisec) :: Double
    cs = ((fromIntegral . clockTickToInteger) (childSystemTime pt) / clockTicksPerMillisec) :: Double

clockTickToInteger (CClock x) = fromIntegral x

-- The ClockTick type is not the "clock" unit that is used by
-- clock(3).  It's the "clock tick" used by times(2).  On my system
-- there seem to be 100 clock ticks per second.
clockTicksPerSec = 100
clockTicksPerMillisec = fromIntegral clockTicksPerSec / 1000.0

    

type Account = IORef (M.Map String Times)

accountInit :: IO Account
accountInit = newIORef M.empty

accountAction :: Account -> String -> IO a -> IO a
accountAction account label action = do
  t1 <- getCPUTime
  pt1 <- getProcessTimes
  result <- action
  t2 <- getCPUTime
  pt2 <- getProcessTimes
  let cpuTimeInMilliseconds = fromIntegral (t2-t1) / (10^9)
      processTimesDiff = pt2 `subtractProcessTimes` pt1
  let times = Times { cpuTime = cpuTimeInMilliseconds
                    , processTimes = processTimesDiff }
  modifyIORef account (M.insertWith' addTimes label times)


  -- internalMap <- readIORef account
  -- let times = M.findWithDefault zeroTimes label internalMap
  --     times' = Times { cpuTime = (cpuTime times) + cpuTimeInMilliseconds
  --                    , processTimes = (processTimes times) `addProcessTimes` processTimesDiff }
  --     internalMap' = M.insert
  -- modifyIORef account (M.insertWith' addTimes label times)
  -- writeIORef account times'
  return result

addTimes :: Times -> Times -> Times
addTimes times1 times2 =
    Times { cpuTime = cpuTime times1 + cpuTime times2
          , processTimes = processTimes times1 `addProcessTimes` processTimes times2 }

combineProcessTimes :: (ClockTick -> ClockTick -> ClockTick) -> ProcessTimes -> ProcessTimes -> ProcessTimes
combineProcessTimes op pt1 pt2 =
    ProcessTimes { elapsedTime = elapsedTime pt1 `op` elapsedTime pt2
                 , userTime = userTime pt1 `op` userTime pt2
                 , systemTime = systemTime pt1 `op` systemTime pt2
                 , childUserTime = childUserTime pt1 `op` childUserTime pt2
                 , childSystemTime = childSystemTime pt1 `op` childSystemTime pt2 }

addProcessTimes = combineProcessTimes (+)
subtractProcessTimes = combineProcessTimes (-)

accountSummary :: Account -> IO ()
accountSummary account = do
  putStrLn "Time accounting summary"
  putStrLn "-----------------------"
  m <- readIORef account
  forM_ (M.toList m) $ \(label, time) -> do
    putStrLn $ label ++ ": " ++ showTimes time ++ "ms"

