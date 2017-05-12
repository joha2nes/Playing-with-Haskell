-- Ripley.hs

module Ripley where

-- types
type Direction = (Float, Float)
type Position = (Float, Float)
data Body = Body { bPos :: (Float, Float)
                 , bVel :: (Float, Float)
                 , bAng :: Float }
data Input = Input { iThrottle :: Float
                   , iSteer :: Float }

-- helpers
clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

move :: Position -> Direction -> Position
move (x, y) (dx, dy) = (x + dx, y + dy)

left :: Direction
left = (-1, 0)

right :: Direction
right = (1, 0)

up :: Direction
up = (0, 1)

down :: Direction
down = (0, -1)

scale :: Direction -> Float -> Direction
scale (x, y) s = (s * x, s * y) 
