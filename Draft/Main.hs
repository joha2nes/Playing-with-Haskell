import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Data.Maybe (fromJust)

import Primitives

vertex3 :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3 (x,y,z) = vertex $ Vertex3 x y z

vertex2 :: (GLfloat, GLfloat) -> IO ()
vertex2 (x,y) = vertex $ Vertex2 x y

red :: IO ()
red = color $ Color3 1 0 (0::GLfloat)

green :: IO ()
green = color $ Color3 0 1 (0::GLfloat)

blue :: IO ()
blue = color $ Color3 0 0 (1::GLfloat)

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints =
	[(-0.25, 0.25, 0.0)
	,(0.75, 0.35, 0.0)
	,(0.75, -0.15, 0.0)
	,((0.95), -0.55, 0.0)]

translateMatrix :: GLfloat -> GLfloat -> GLfloat -> [GLfloat]
translateMatrix x y z = [1, 0, 0, x, 0, 1, 0, y, 0, 0, 1, z, 0, 0, 0, 1]

shear :: GLfloat -> IO ()
shear f = do
	m <- newMatrix RowMajor [ 1, f, 0, 0
							, 0, 1, 0, 0
							, 0, 0, 1, 0
							, 0, 0, 0, 1 ]
	multMatrix (m :: GLmatrix GLfloat)


display :: Window -> IO ()
display window = do
	clearColor $= Color4 0 0 0 1
	--clear [ColorBuffer]
	
	time <- getTime
	let t = realToFrac (fromJust time)

	preservingMatrix $ do
		rotate' (t * 200)
		translate $ Vector3 0 0.5 (0::GLfloat)
		rotate' (t * 800)
		translate $ Vector3 0 0.2 (0::GLfloat)
		rotate' (t * (-2200))
		translate $ Vector3 0 0.2 (0::GLfloat)
		rotate' (t * 4500)
		translate $ Vector3 0 0.1 (0::GLfloat)
		color $ Color3 (sin t) (cos t) (tan t)
		renderPrimitive Points point

	swapBuffers window
	where
		scale' :: GLfloat -> IO ()
		scale' a = scale a a a
		rotate' :: GLfloat -> IO ()
		rotate' a = rotate a $ Vector3 0 0 1

loop :: Window -> IO ()
loop window = do
	display window
	pollEvents
	closeWindow <- windowShouldClose window
	if closeWindow then destroyWindow window else loop window

main = do
	GLFW.init
	window <- createWindow 800 600 "some game" Nothing Nothing
	makeContextCurrent window
	--polygonMode $= (Line, Line)
	pointSize $= 1
	--lineStipple $= Just (1,255)
	--lineWidth $= 10
	loop (fromJust window)
	terminate