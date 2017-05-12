module Vector ( Vector ) where

data Vector = Vector Float Float

instance Show Vector where
	show (Vector x y) = "<" ++ (show x) ++ ", " ++ (show y) ++ ">"

instance Num Vector where
	(+) (Vector a b) (Vector c d) = Vector (a + c) (b + d)
	(-) (Vector a b) (Vector c d) = Vector (a - c) (b - d)


vLength :: Vector -> Float
vLength (x, y) = sqrt (x^2 + y^2)

vPlus :: Vector -> Vector -> Vector
(x1, y1) `vPlus` (x2, y2) = (x1 + x2, y1 + y2)