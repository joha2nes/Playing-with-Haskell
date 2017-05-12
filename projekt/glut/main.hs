import Graphics.UI.GLUT
import Data.IORef

data GameState = Game { rotation :: GLfloat } deriving (Show)


initialGame :: GameState
initialGame = Game 0

vertex2 :: (GLfloat, GLfloat) -> IO ()
vertex2 (x, y) = vertex (Vertex2 x y)

color3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3 r g b = color (Color3 r g b)


main = do
	-- initialize GLUT/OpenGL
	getArgsAndInitialize
	
	-- create window
	createWindow "Hello"

	-- create mutable varibles
	newGame <- newIORef initialGame
	currentTime <- get elapsedTime
	newTime <- newIORef currentTime
	newInput <- newIORef ()
	
	-- set callbacks
	addTimerCallback 20 (loop newGame newTime)
	specialCallback $= Just (input newGame)

	-- openGL loop
	mainLoop


loop :: IORef GameState -> IORef Int -> IO ()
loop game time = do
	-- set next callback
	addTimerCallback 20 (loop game time)

	-- calculate delta time
	oldTime <- readIORef time
	currentTime <- get elapsedTime
	let dt = fromIntegral (currentTime - oldTime) / 1000

	-- update
	update game dt

	-- output
	display game


update :: IORef GameState -> GLfloat -> IO ()
update game dt = do 
	(Game a) <- get game
	game $= Game (a + dt)


display :: IORef GameState -> IO ()
display game = do
	(Game angle) <- get game
	clearColor $= Color4 0.1 0 0.2 1
	clear [ColorBuffer]
	rotate (-2) $ Vector3 0 0 (angle :: GLfloat)
	preservingMatrix $ do
		renderPrimitive Triangles $ do
			color3 1 1 0
			vertex2 (-0.5, -1)
			color3 0 1 1
			vertex2 (0, 0)
			color3 1 0 1
			vertex2 (0.5, -1)
	flush


input :: IORef GameState -> SpecialCallback
input game k _ = return ()