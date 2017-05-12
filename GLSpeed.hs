import Graphics.UI.GLFW as GLFW
import Data.Maybe (fromJust)
import Graphics.Rendering.OpenGL as GL

-- types
type Time = Double

-- functions
createShaders :: IO ()
createShaders = do
	vs <- createShader VertexShader
	fs <- createShader FragmentShader
	vsSource <- readFile "shaders/basic.vs"
	fsSource <- readFile "shaders/basic.fs"
	

mainLoop :: Window -> Time -> IO ()
mainLoop window oldTime = do
	currentTime <- getTime
	let deltaTime = (fromJust currentTime) - oldTime
	--	
	clearColor $= Color4 0 0 0 1
	clear [ColorBuffer]
	print $ round (1.0 / deltaTime)
	preservingMatrix $ do
		translate $ Vector3 (-0.6) 0 (0::GLfloat)
		renderPrimitive Points $ vertex (Vertex2 0 (0::GLfloat))
	--
	swapBuffers window
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