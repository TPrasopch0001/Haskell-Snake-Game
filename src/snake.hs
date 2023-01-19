{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Snake where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe
import Data.Ord hiding (Down)
import Data.List ()
import Data.List.Split (chunksOf)
import System.Random
import System.Random.Shuffle ()
import Control.Monad.State
import System.IO
import System.Console.ANSI
import Control.Concurrent
import GHC.IO
import qualified Data.Set as S

type WallLoc = [Coords]
type PointLoc = [Coords]
type Coords = (Int, Int)
type Dims = (Int, Int)
data Direction = L | R | U | D deriving (Eq, Show, Ord)
data World = World {dims :: Dims, curLoc :: Coords, curLength :: Int, tailLoc :: [Coords] ,dir :: Direction, pointLoc :: PointLoc, inPlay :: Bool, pause :: Bool}

randPairs :: (RandomGen g, Random a, Num a) => (a,a) -> g -> [(a,a)] --Max Vals as parameters
randPairs (w,h) gen = zip as bs
    where
        (a,b) = split gen      -- create two separate generators
        as = randomRs (1,w-1) a  -- one infinite list of randoms
        bs = randomRs (1,h-1) b  -- another

coords :: Int -> Dims -> [(Int,Int)] --Generates Coordinates Using width/height parameters
coords count d@(w,h)= take count $              -- 50 random coordinates derived
    randPairs (w,h) mygen
        where
            mygen  = mkStdGen (uncurry (+) d `div`2)  -- from the random seed above

emptyWorld :: Dims -> Coords -> Int ->World --Default World - Dimensions, Starting Coordinate and Total Points Generated As Parameters
emptyWorld dims start points= World dims start 1 [start] R (coords points dims) True True

checkPt :: World -> World -- Checks If Current Snake Head Coords Are The Same As Current Point Location - Updates Point List And Snake Length If Same
checkPt wld@(World _ c@(x, y) len ts _ ps _ _)
  | c == head ps =
        wld{curLength = len+1, pointLoc = tail ps, tailLoc = ts++[c]}
  | otherwise = wld

drawWorld :: World -> Picture
drawWorld wld@(World (w, h) c@(curX, curY) len ts _ ps go pause) =
    if go
        then
            if not (null ps)
                then
                    translate (-fromIntegral w*20) (fromIntegral h*20) $ scale (fromIntegral w*3.5) (fromIntegral h*3.5) display
                else
                    translate (-10) 0 $ scale 14 14 winScreen
        else
            translate (-10) 0 $ scale 14 14 loseScreen
                where
                    pointText = "Points:" ++ show (len-1)
                    winScreen = pictures [color white $ rectangleSolid 40 40, color blue $ translate (- 13.75) 0 $ scale 0.05 0.05 $ text "You Win!", color blue $ translate (- 6) (- 5) $ scale 0.03 0.03 $ text pointText]
                    loseScreen = pictures [color white $ rectangleSolid 40 40, color blue $ translate (- 14.5) 0 $ scale 0.04 0.05 $ text "Game Over!", color blue $ translate (- 6) (- 5) $ scale 0.03 0.03 $ text pointText]
                    display =
                        pictures $ [square row colum | row <- [0 .. w], colum <- [0 .. h]] ++ [color white $ translate 0 (- 12.5) $ scale 0.01 0.01 $ text pointText]
                        where
                            square x y
                                = color
                                    (if (x, y) == c || (x,y) `elem` ts
                                        then
                                            rainbowColors (x,y)
                                        else
                                            if (x, y) == head ps
                                                then
                                                    dark red
                                                else
                                                    if even (x + y)
                                                        then makeColorI 66 23 0 150
                                                        else makeColorI 94 34 2 150)
                                    (translate
                                        (fromIntegral x + 0.5) (- fromIntegral y - 0.5)
                                        $ rectangleSolid 1 1)

rainbowColors :: (Int, Int) -> Graphics.Gloss.Interface.IO.Game.Color
rainbowColors (x,y)
    | c == 1 = red 
    | c == 2 = orange 
    | c == 3 = yellow
    | c == 4 = green
    | c == 5 = cyan 
    | c == 6 = blue
    | otherwise = magenta
        where c = (x+y) `mod` 7
checkMove :: Coords -> Coords -> World -> Bool
checkMove (x,y) (x',y') wld@(World (w,h) _ _ ts _ _ _ _) =
    c `notElem` ts && dx > -1 && dx <= w && dy > -1 && dy <= h
        where c@(dx,dy) = (x+x',y+y')

handleEvents :: Event  -> World -> World
handleEvents (EventKey (Char 'r') Down _ _) _ = emptyWorld (10,10) (1,9) 100 --Restarts Game (Use same function as in Main)
handleEvents (EventKey (Char 'p') Down _ _) wld@(World _ c@(x,y) _ _ _ _ _ stop) = if stop then wld {pause = False} else wld {pause = True} --Handles Pausing/Unpausing function
handleEvents (EventKey (Char 'w') Down _ _) wld@(World _ c@(x,y) _ _ dir _ _ _) = if dir /= D then wld{ dir = U} else wld
handleEvents (EventKey (Char 'a') Down _ _) wld@(World _ c@(x,y) _ _ dir _ _ _) = if dir /= R then wld{ dir = L} else wld
handleEvents (EventKey (Char 's') Down _ _) wld@(World _ c@(x,y) _ _ dir _ _ _) = if dir /= U then wld{ dir = D} else wld
handleEvents (EventKey (Char 'd') Down _ _) wld@(World _ c@(x,y) _ _ dir _ _ _) = if dir /= L then wld{ dir = R} else wld
handleEvents (EventKey (SpecialKey KeyUp) Down _ _) wld@(World _ c@(x,y) _ _ dir _ _ _) = if dir /= D then wld{ dir = U} else wld
handleEvents (EventKey (SpecialKey KeyLeft) Down _ _) wld@(World _ c@(x,y) _ _ dir _ _ _) = if dir /= R then wld{ dir = L} else wld
handleEvents (EventKey (SpecialKey KeyDown) Down _ _) wld@(World _ c@(x,y) _ _ dir _ _ _) = if dir /= U then wld{ dir = D} else wld
handleEvents (EventKey (SpecialKey KeyRight) Down _ _) wld@(World _ c@(x,y) _ _ dir _ _ _) = if dir /= L then wld{ dir = R} else wld
handleEvents _ world = world

nextWorld :: World -> World
nextWorld world@(World _ c@(x,y) _ ts dir ps go stop)
    | stop = world --Freezes World When Paused
    | null ps = world --Freezes World When No More Points Exist (Used To Keep "You Win" Screen Open)
    | dir == U = if checkMove (x,y) (0,-1) world then checkPt $ world { curLoc = (x,y-1), tailLoc =tail ts ++ [c]} else world { curLoc = (x,y-1), tailLoc =tail ts ++ [c],inPlay = False}
    | dir == D = if checkMove (x,y) (0,1) world then checkPt $ world { curLoc = (x,y+1), tailLoc =tail ts ++ [c]} else world {curLoc = (x,y+1), tailLoc =tail ts ++ [c],inPlay = False}
    | dir == L = if checkMove (x,y) (-1,0) world then checkPt $ world { curLoc = (x-1,y),tailLoc =tail ts ++ [c]} else world {curLoc = (x-1,y),tailLoc =tail ts ++ [c], inPlay = False}
    | dir == R = if checkMove (x,y) (1,0) world then checkPt $ world { curLoc = (x+1,y), tailLoc =tail ts ++ [c]} else world {curLoc = (x+1,y), tailLoc =tail ts ++ [c],inPlay = False}

updateVals :: Coords -> [Coords] -> [Coords]
updateVals _ [] = []
updateVals (dx,dy) ((x,y):ls) = (x+dx,y+dy):updateVals (dx,dy) ls
