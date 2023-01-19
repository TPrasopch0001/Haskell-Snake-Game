module Main where
import Snake
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe
import Data.Ord
import Data.List hiding (lines)
import Data.List.Split (chunksOf)
import System.Random
import System.Random.Shuffle
import Control.Monad.State
import System.IO
import System.Console.ANSI
import Control.Concurrent
import GHC.IO

main :: IO ()
main = play (InWindow "Snake" (500,500) (0,0))
            black
            7
            (emptyWorld (10,10) (1,9) 100)
            drawWorld
            handleEvents
            (\_ -> nextWorld)

-- main :: IO ()
-- main = play (InWindow "MP2" (600,400) (0,0))
--             black
--             10
--             (defaultWorld [(1,1)])
--             drawWorld
--             handleEvents
--             (\_ -> nextWorld)


-- main :: IO ()
-- main = display (InWindow "MP2" (500,500) (0,0)) white (scale 10 10 (drawWorld $ emptyWorld (10,10)))