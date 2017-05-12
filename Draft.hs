import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Data.Maybe (fromJust)

vertex3 :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3 (x,y,z) = vertex $ Vertex3 x y z

vertex2 :: (GLfloat, GLfloat) -> IO ()
vertex2 (x,y) = vertex $ Vertex2 x y

circle :: GLfloat -> GLfloat -> [(GLfloat, GLfloat)]
circle r n = [(sin(2 * pi * x / n) * r, cos(2 * pi * x / n) * r) | x <- [1..n] ]

drawCircle :: GLfloat -> IO ()
drawCircle r = renderPrimitive LineLoop . mapM_ vertex2 $ circle r 100

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
	clear [ColorBuffer]

	loadIdentity
	shear 1
	drawCircle 0.1

	swapBuffers window

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
	--pointSize $= 6
	--lineStipple $= Just (1,255)
	--lineWidth $= 10
	loop (fromJust window)
	terminate