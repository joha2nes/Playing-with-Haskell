import Asteroids
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.ByteString as Bs

spacecraft :: Picture
spacecraft = color white $ polygon [(-5,-10),(0,10),(5,-10)]

laser :: Picture
laser = color (light red) $ line [(0,0),(0,-10)]

asteroid :: Picture
asteroid = color orange $ circleSolid 30  

drawObject :: Object -> Picture -> Picture
drawObject (Object (x, y) _ a) = translate x y . rotate a


draw :: World -> Picture
draw (World craft lasers rocks _) = pictures
	[ drawObject craft spacecraft
	, pictures $ map (`drawObject` laser) lasers
	]
	where
		(cX, cY) = oPos craft
		cA = oAng craft

nullInput :: Input
nullInput = Input False False False False


input :: Event -> World -> World
input (EventKey (SpecialKey KeyLeft) Down _ _) w = updateWorld 0.0 (Input True False False False) w 
input (EventKey (SpecialKey KeyRight) Down _ _) w = updateWorld 0.0 (Input False True False False) w
input (EventKey (SpecialKey KeyUp) Down _ _) w = updateWorld 0.0 (Input False False True False) w
input (EventKey (SpecialKey KeyDown) Down _ _) w = updateWorld 0.0 (Input False False False True) w
input _ w = w

update :: Float -> World -> World
update t w = updateWorld t nullInput w

main = do
	content <- Bs.readFile "textures/spacecraft.png"
	bitmap 

--main = play (InWindow "Asteroids" (800, 600) (100, 100)) black 60 initialWorld draw input update