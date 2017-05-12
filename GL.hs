import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Data.Maybe (fromJust)

vertex3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3 x y z = vertex $ Vertex3 x y z

vertex2 :: GLfloat -> GLfloat -> IO ()
vertex2 x y = vertex $ Vertex2 x y

color3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3 r g b = color $ Color3 r g b

square :: GLfloat -> IO ()
square w = renderPrimitive Quads $ do
	vertex2 (-w/2) (-w/2)
	vertex2 (-w/2) (w/2)
	vertex2 (w/2) (w/2)
	vertex2 (w/2) (-w/2)

errorCallback :: ErrorCallback -- Error -> String -> IO ()
errorCallback err msg = putStr $ "Error! " ++ (show err) ++ ", " ++ msg

keyCallback :: KeyCallback -- Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback window key code action mods = return ()

loop :: Window -> IO ()
loop window = do
	clearColor $= Color4 0.2 0.3 0.5 1.0
	clear [ColorBuffer]

	renderPrimitive Triangles $ do
		color3 1 0 0
		vertex3 (-0.5) (-0.5) 0
		color3 0 1 0
		vertex3 0.5 (-0.5) 0
		color3 0 0 1
		vertex3 (-0.5) 0.5 0

	color3 1 1 1
	rotate 1 $ Vector3 0 0 (1::GLfloat)
	square 0.1

	swapBuffers window

	pollEvents
	closeWindow <- windowShouldClose window
	if closeWindow then destroyWindow window else loop window

main = do
	setErrorCallback (Just errorCallback)
	initialized <- GLFW.init
	if not initialized then print "could not initialize GLFW"
	else do
		window <- createWindow 800 600 "window" Nothing Nothing
		if window == Nothing then print "could not create a window" >> terminate
		else do
			makeContextCurrent window
			swapInterval 1
			setKeyCallback (fromJust window) (Just keyCallback)
			loop (fromJust window)
			terminate