-- RipleyGL.hs

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Data.Maybe (fromJust)
import Ripley as Rip

-- types
type Time = Double

-- functions
display :: Window -> Rip.Position -> IO ()
display window pos = do
	clearColor $= Color4 0 0 0 1
	clear [ColorBuffer]
	loadIdentity

	let (x, y) = pos
	    x' = realToFrac x :: GLfloat
	    y' = realToFrac y :: GLfloat
	pointSize $= 30
	preservingMatrix . renderPrimitive Points . vertex $ Vertex2 x' y'

	swapBuffers window
	
getInput :: IO Rip.Direction
getInput = do
	connected <- joystickPresent Joystick'1
	if not connected then return (0, 0)
		else do
			maybeAxes <- getJoystickAxes Joystick'1
			let [a,b,c,d,e] = fromJust maybeAxes
			    x = realToFrac a :: Float
			    y = realToFrac b :: Float
			return (x*0.001, -y*0.001)


mainLoop :: Window -> Time -> Rip.Position -> IO ()
mainLoop window oldTime oldPos = do
	currentTime <- getTime
	let deltaTime = (fromJust currentTime) - oldTime
	    fps = round (1 / deltaTime)
	print fps
	--
	dir <- getInput
	let newPos = move oldPos dir
	display window newPos
	--
	pollEvents
	closeWindow <- windowShouldClose window
	if closeWindow then destroyWindow window else mainLoop window (fromJust currentTime) newPos

-- MAIN
main = do
	initialized <- GLFW.init
	if not initialized then print "could not initialize GLFW"
	else do
		window <- createWindow 800 600 "window" Nothing Nothing
		if window == Nothing then print "could not create a window" >> terminate
		else do
			makeContextCurrent window
			swapInterval 0
			time <- getTime
			mainLoop (fromJust window) (fromJust time) (0, 0)
			terminate