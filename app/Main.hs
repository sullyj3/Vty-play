{-# Language LambdaCase #-}
{-# Language BlockArguments #-}
{-# Language DeriveFunctor #-}
{-# Language NamedFieldPuns #-}
{-# Language OverloadedStrings #-}

-- | Turns out, terminal can't detect keyup events. This is a deal breaker for realtime games


module Main where

import Graphics.Vty
import System.Clock
import Linear.V2

import Lib

data GameState = GS { position :: V2 Int
                    , lastTime :: TimeSpec
                    }

worldBounds :: AABB Int
worldBounds = AABB (V2 0 0) (V2 70 30)

width :: Num a => AABB a -> a
width (AABB (V2 x1 _) (V2 x2 _)) = x2 - x1

height :: Num a => AABB a -> a
height (AABB (V2 _ y1) (V2 _ y2)) = y2 - y1

data Direction = U
               | D
               | L
               | R

unitDir :: Direction -> V2 Int
unitDir = \case
  L -> V2 (-1) 0
  R -> V2 1 0
  U -> V2 0 (-1)
  D -> V2 0 (1)

data AABB a =
  AABB { topLeft     :: V2 a
       , bottomRight :: V2 a
       }


restrict :: Ord a => AABB a -> V2 a -> V2 a
restrict (AABB (V2 left bottom) (V2 right top)) (V2 x y) =
  V2 (clamp left x right) (clamp bottom y top)

clamp :: Ord a => a -> a -> a -> a
clamp min x max
  | x < min   = min
  | x > max   = max
  | otherwise = x

move :: V2 Int -> Direction -> V2 Int
move pos dir = restrict worldBounds $ pos + unitDir dir

data GameEnv = Env { vty :: Vty }

main :: IO ()
main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg

  initialTime <- getTime Monotonic
  let gamesState = GS { position = V2 3 3, lastTime = initialTime }

  finalState <- runGame (Env vty) gamesState game
  --mainLoop vty (picForImage emptyImage)  initialTime

  shutdown vty

type Game a = ReaderT GameEnv (StateT GameState IO) a

runGame :: GameEnv
        -> GameState
        -> Game a
        -> IO (a, GameState)
runGame env state game = runStateT (runReaderT game env) state

game :: Game ()
game = do
  Env { vty } <- ask
  gs <- get
  liftIO . update vty . draw $ gs

  (liftIO $ nextEventNonblocking vty) >>= \case
    Just e -> case e of
      (EvKey (KChar 'q') []) -> return ()
      (EvKey (KUp) [])    -> modify (stateMove U) >> game
      (EvKey (KDown) [])  -> modify (stateMove D) >> game
      (EvKey (KLeft) [])  -> modify (stateMove L) >> game
      (EvKey (KRight) []) -> modify (stateMove R) >> game
      _                      -> game
    Nothing -> game

stateMove :: Direction -> GameState -> GameState
stateMove dir gs = gs { position = move (position gs) dir }

draw :: GameState -> Picture
draw gs = picForLayers [ character (position gs) 
                       , backDrop
                       ]

mainLoop :: Vty -> Picture -> TimeSpec -> IO ()
mainLoop vty pic lastTime = do
  unlessM (quit vty)
    do
      t <- getTime Monotonic
      let dt = t - lastTime
      update vty pic
      let line  = makeLine ("Current time is: " ++ show t)
      let line2 = makeLine ("Frame Delta: " <> show (ms dt) <> "ms")
      let image = line <-> line2
      mainLoop vty (picForImage image) t


makeLine s = string (defAttr `withBackColor` blue) s


character :: V2 Int -> Image
character (V2 x y) = translate x y $ char defAttr '■'


backDrop :: Image
backDrop = vertCat . map (string defAttr) $
       [topRow]
    <> replicate (h - 1) row
    <> [bottomRow]
  where
  w = width worldBounds
  h = height worldBounds

  topRow    = "┏" <> replicate (w - 1) '━' <> "┓"
  row       = "┃" <> replicate (w - 1) ' ' <> "┃"
  bottomRow = "┗" <> replicate (w - 1) '━' <> "┛"


quit :: Vty -> IO Bool
quit vty = nextEventNonblocking vty >>= \case
  Just (EvKey (KChar 'q') []) -> return True
  _ -> return False

ms :: TimeSpec -> Double
ms (TimeSpec {sec, nsec}) = (realToFrac sec * 1000) + (realToFrac nsec / 1000000)
