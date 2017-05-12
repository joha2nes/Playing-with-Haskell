-- Geometry3D.hs


module Geometry3D
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidVolume
, cuboidArea
) where

-- [sphere]

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius^3)

sphereArea :: Float -> Float
sphereArea radius = 4.0 * pi * (radius^2)

-- [cube]

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

-- [cuboid]

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea b c * 2



-- non-exported functions / helper functions

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b