-- Primitives.hs

module Primitives where 
import Graphics.Rendering.OpenGL

point :: IO ()
point = vertex $ Vertex3 0 0 (0::GLfloat) 

square :: IO ()
square = do
	vertex $ Vertex3 w w 0
	vertex $ Vertex3 w (-w) 0
	vertex $ Vertex3 (-w) (-w) 0
	vertex $ Vertex3 (-w) w 0
	where w = 0.5::GLfloat

circle :: IO ()
circle = mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) [(sin(2 * pi * x / n) * 0.5, cos(2 * pi * x / n) * 0.5, 0) | x <- [1..n] ]
	where n = 50::GLfloat
