import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Data.Maybe (fromJust)

-- types
type Time = Double

-- functions
display :: Window -> IO ()
display window = do
	clearColor $= Color4 0 0 0 1
	clear [ColorBuffer]
	loadIdentity

	connected <- joystickPresent Joystick'1
	if not connected then return ()
		else do
			maybeAxes <- getJoystickAxes Joystick'1
			let  [a,b,c,d,e] = fromJust maybeAxes
			     x1 = realToFrac a :: GLfloat
			     y1 = realToFrac b :: GLfloat
			     bump = realToFrac c :: GLfloat
			     y2 = realToFrac d :: GLfloat
			     x2 = realToFrac e :: GLfloat
			color $ Color3 0.4 1 (0.1::GLfloat)
			pointSize $= 10
			preservingMatrix $ do
				translate $ Vector3 (-0.6) 0 (0::GLfloat)
				renderPrimitive Points $ vertex (Vertex2 (x1/5) ((-y1)/5))
			preservingMatrix $ do
				translate $ Vector3 (0.5) (-0.3) (0::GLfloat)
				renderPrimitive Points $ vertex (Vertex2 (x2/5) ((-y2)/5))
			pointSize $= 20
			preservingMatrix $ do
				translate $ Vector3 0 0.5 (0::GLfloat)
				renderPrimitive Points $ vertex $ Vertex2 ((-bump)/5) 0
	swapBuffers window

mainLoop :: Window -> Time -> IO ()
mainLoop window oldTime = do
	pollEvents

	currentTime <- getTime
	let deltaTime = (fromJust currentTime) - oldTime
	    fps = round (1 / deltaTime)
	print fps

	display window
	
	closeWindow <- windowShouldClose window
	if closeWindow then destroyWindow window else mainLoop window (fromJust currentTime)

-- MAIN
main = do
	initialized <- GLFW.init
	if not initialized then print "could not initialize GLFW"
	else do
		window <- createWindow 800 400 "window" Nothing Nothing
		if window == Nothing then print "could not create a window" >> terminate
		else do
			makeContextCurrent window
			time <- getTime
			swapInterval 1
			mainLoop (fromJust window) (fromJust time)
			terminate