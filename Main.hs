module Main where
import System.IO.Unsafe
import System.Random
import qualified Data.Set as Set

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

data Ant   = Ant  { ap :: Point , hasFood :: Bool } deriving Show
data Cell  = Cell { cp :: Point , pheromone :: Float } deriving Show
data Home  = Home { hp :: Point , food :: Int } deriving Show
data Food  = Food { fp :: Point } deriving Show
data World = World { frame  :: Int
                   , home   :: Home
                   , foods  :: Set.Set Point
                   , ants   :: [ Ant  ]
                   , cells  :: [ Cell ]
                   } deriving Show

width, height, cellCount, antCount :: Int
width  = 600
height = 600
cellCount = 50
antCount = 40
foodCount = 60

homePos = (25, 25)

cellSize :: Int
cellSize = div width cellCount

initWorld = World { frame  = 0
                  , home   = Home { hp = homePos, food = 0 }
                  , foods  = Set.fromList $ makeFood foodCount
                  , ants   = makeAnts antCount homePos
                  , cells  = makeCells (fromIntegral cellCount) (fromIntegral cellCount)
                  }

makeCells w h = map mc [(x, y) | x <- [0..w], y <- [0..h]]
                where mc p = Cell { cp = p, pheromone = 0 } --getRandom (0.0, 1.0) }

makeAnts num hp = map (\x -> Ant { ap = hp, hasFood = False }) [0..num]
makeFood num = map (\x -> ( fromIntegral $ getRandom (0, cellCount)
                          , fromIntegral $ getRandom (0, cellCount)
                          )) [0..num]

getRandom range = unsafePerformIO $ getStdRandom (randomR range)

main :: IO ()
main = do
        simulate (InWindow "Ant Simulation" (width, height) (0, 0))
                black
                20
                initWorld
                drawWorld
                stepWorld

stepWorld :: ViewPort -> Float -> World -> World
stepWorld _ _ w = w { frame = 1 + frame w
                    , ants = map (updateAnt w) (ants w) 
                    }

drawWorld :: World -> Picture
drawWorld w = Pictures
              $ map (Translate (-300) (-300))
              $ (drawCells (fromIntegral cellSize) w) ++
                (drawAnts (fromIntegral cellSize) w) ++
                (drawFoods (fromIntegral cellSize) w) ++
                [ drawFrames w
                , drawHome (fromIntegral cellSize) w
                ]

drawFrames w = Scale 0.1 0.1
               $ Color red
               $ Text
               $ "Frame: " ++ (show $ frame w)

randomWalk (x, y) = case (getRandom (0, 3)) :: Int of
    0 -> (x + 1, y)
    1 -> (x - 1, y)
    2 -> (x, y + 1)
    3 -> (x, y - 1)

goHome (ax, ay) (hx, hy) = let x = ax - hx
                               y = ay - hy
                           in if (abs x) > (abs y) then (ax - (x / (abs x)), ay)
                                                   else (ax, ay - (y / (abs y)))

updateAnt w a@Ant { ap = ap, hasFood = f } =
    if f && (ap == homePoint) then a { hasFood = False }
    else if f then a { ap = goHome ap homePoint }
    else if (not f) && (Set.member ap fs) then a { ap = goHome ap homePoint, hasFood = True }
    else a { ap = randomWalk ap }
    where homePoint = hp $ home w
          fs        = foods w

pheromoneColor p       = makeColor p p p 1
coordToPos size (x, y) = (x * size, y * size)
rect size (x, y)       = Polygon [(x, y), (x, y + s), (x + s, y + s), (x + s, y)]
    where s = fromIntegral size
drawHome size w        = Color red $  rect cellSize $ coordToPos size (hp $ home w)
drawCells size w       = map (drawCell size) $ filter (\c -> (0 /= pheromone c)) $ cells w
drawCell size (Cell { cp = cp, pheromone = p }) =
    Color (pheromoneColor p) $ rect cellSize (coordToPos size cp)
drawAnts size w        = map (drawAnt size) $ ants w
drawAnt size Ant { ap = ap, hasFood = f } = (foodColor f) $ rect cellSize (coordToPos size ap)
foodColor f = case f of
    True -> Color green
    False -> Color blue
drawFoods size w = map (drawFood size) $ Set.toList $ foods w
drawFood size p = Color yellow $ rect cellSize $ (coordToPos size p)
