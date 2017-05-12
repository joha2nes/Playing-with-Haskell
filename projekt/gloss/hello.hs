import Graphics.Gloss

path n size = [ (size * (sin (2*pi*k/n) + 0.2 * cos (negate (50*pi*k/n))), size * cos (2*pi*k/n)) | k <- [1..n] ]
	where
		x k = 
main = display (InWindow "something" (1000, 800) (100, 100)) black . color (light blue) . polygon $ path 2000 100