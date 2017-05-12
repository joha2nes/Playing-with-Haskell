-- AStar.hs

{- module for A*-star pathfinding alghoritm -}

-- types

data Node = Node 
	{ position :: (Float, Float)
	, passable :: Bool
	, f :: Float
	, h :: Float
	, g :: Float 
	}

