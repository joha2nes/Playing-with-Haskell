module Asteroids 
( Position
, Velocity
, Angle
, Time
, Object(..)
, World(..)
, Input(..)
, initialWorld
, updateWorld
) where

import Data.Tuple (swap)

type Vector = (Float, Float)
type Position = (Float, Float)
type Velocity = (Float, Float)
type Angle = Float
type Time = Float

data Object = Object { oPos :: Position
					 , oVel :: Velocity
					 , oAng :: Angle
					 } deriving (Show)

data World = World { wCraft  :: Object
				   , wLasers :: [Object]
				   , wRocks  :: [Object]
				   , wScore  :: Int
				   } deriving (Show)

data Input = Input { iLeft     :: Bool
				   , iRight    :: Bool
				   , iThrottle :: Bool
				   , iShoot    :: Bool
				   }

initialWorld :: World
initialWorld = World { wCraft = Object (0, 0) (0, 0) 0
					 , wLasers = []
					 , wRocks = []
					 , wScore = 0
					 }


-- helpers
vAdd :: Vector -> Vector -> Vector
vAdd (a, b) (c, d) = (a+c, b+d)

vSub :: Vector -> Vector -> Vector
vSub (a, b) (c, d) = (a-c, b-d)

vScale :: Float -> Vector -> Vector
vScale s (x, y) = (x*s, y*s)

angToVec :: Angle -> Vector
angToVec a = swap (cos (a*pi/180), sin (a*pi/180))



updateObj :: Time -> Object -> Object
updateObj t obj = obj { oPos = pos' }
	where
		pos'     = (x + vx * t, y + vy * t)
		(x, y)   = oPos obj
		(vx, vy) = oVel obj

rotateObj :: Object -> Angle -> Object
rotateObj obj a = obj { oAng = a' }
	where
		a' = oAng obj + a

updateWorld :: Time -> Input -> World -> World
updateWorld t input oldWorld = World craft''' lasers'' rocks' score'
	where
		craft''' = updateObj t craft''
		craft'' = craft' { oVel = (oVel craft') `vAdd` (vScale 50 force) }
		craft' = rotateObj (wCraft oldWorld) degrees
		
		force = if iThrottle input then angToVec (oAng (wCraft oldWorld)) else (0, 0)
		degrees | iLeft input  = -20
				| iRight input = 20
			    | otherwise    = 0

		lasers'' = map (updateObj t) lasers'
		lasers' = if iShoot input 
					then Object (oPos (wCraft oldWorld)) (10, 10) 0 : wLasers oldWorld
					else wLasers oldWorld

		rocks' = map (updateObj t) (wRocks oldWorld)
		
		score' = wScore oldWorld + 1