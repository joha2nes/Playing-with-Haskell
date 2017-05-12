import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Data.Maybe (fromJust)

-- types
type Time = Double

-- functions
mainLoop :: Window -> Time -> IO ()
mainLoop window oldTime = do
	currentTime <- getTime
	let deltaTime = (fromJust currentTime) - oldTime
	--

	--
	pollEvents
	closeWindow <- windowShouldClose window
	if closeWindow then destroyWindow window else mainLoop window (fromJust currentTime)

-- MAIN
main = do
	initialized <- GLFW.init
	if not initialized then print "could not initialize GLFW"
	else do
		window <- createWindow 800 600 "window" Nothing Nothing
		if window == Nothing then print "could not create a window" >> terminate
		else do
			makeContextCurrent window
			time <- getTime
			mainLoop (fromJust window) (fromJust time)
			terminate