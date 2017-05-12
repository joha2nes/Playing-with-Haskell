import FRP.Yampa

type Pos = Double
type Vel = Double

fallingBall :: Pos -> SF () (Pos, Vel)
fallingBall pos = constant pos &&& (constant (-9.81) >>> integral)

main = return ()