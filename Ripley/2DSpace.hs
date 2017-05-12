

type Vector = (Float, Float)
type Point = (Float, Float)

addVec :: Vector -> Vector -> Vector
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subVec :: Vector -> Vector -> Vector
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

mulVec :: Vector -> Vector -> Vector
mulVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

translate :: Vector -> Point -> Point
translate (vx, vy) (px, py) = (px + vx, py + vy)
