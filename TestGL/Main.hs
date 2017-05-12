import Graphics.UI.GLFW as GLFW
import Data.Maybe (fromJust)
import Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString as BS

-- types
type Time = Double

-- functions
createShaders :: IO ()
createShaders = do
	-- shaders
	vs <- createShader VertexShader
	fs <- createShader FragmentShader
	vsSource <- BS.readFile "shaders/basic.vs"
	fsSource <- BS.readFile "shaders/basic.fs"
	shaderSourceBS vs $= vsSource
	shaderSourceBS fs $= fsSource
	compileShader vs
	compileShader fs
	vsStatus <- get (compileStatus vs)
	fsStatus <- get (compileStatus fs)
	putStrLn $ "- vertex shader compile status = " ++ (show vsStatus)
	putStrLn $ "- fragment shader comiple status = " ++ (show fsStatus)
	-- program
	program <- createProgram
	attachShader program vs
	attachShader program fs
	linkProgram program
	linkStat <- get (linkStatus program)
	putStrLn $ "- program linking status = " ++ (show linkStat)
	currentProgram $= (Just program)
	return ()

mainLoop :: Window -> Time -> IO ()
mainLoop window oldTime = do
	currentTime <- getTime
	let deltaTime = (fromJust currentTime) - oldTime
	--	
	clearColor $= Color4 0 0 0 1
	clear [ColorBuffer]
	--print $ round (1 / deltaTime)
	preservingMatrix $ do
		translate $ Vector3 (-0.6) 0 (0::GLfloat)
		renderPrimitive Quads $ do
			vertex (Vertex2 0 (0::GLfloat))
			vertex (Vertex2 0.1 (0::GLfloat))
			vertex (Vertex2 0.1 (0.1::GLfloat))
			vertex (Vertex2 0 (0.1::GLfloat))
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
			createShaders
			mainLoop (fromJust window) (fromJust time)
			terminate