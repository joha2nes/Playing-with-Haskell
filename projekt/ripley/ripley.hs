-- ripley.hs

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- data

type Position = (Float, Float)
type Velocity = (Float, Float)
type Angle = Float

data Player { pPos :: Position
            , pAng :: Angle
            , pLVel :: Velocity
            , pAVel :: Float
            }

data World = 