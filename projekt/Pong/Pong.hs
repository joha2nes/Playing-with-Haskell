
type Position = (Int, Int) 
type Velocity = (Int, Int)

data Paddle = Paddle Int
data Ball = Ball Position Velocity

data Game = Game { gBall :: Ball
                 , gPaddle1 :: Paddle
                 , gPaddle2 :: Paddle
                 , gScore1 :: Int
                 , gScore2 :: Int }

data Input = Up | Down | None

movePaddle :: Int -> Input -> Int
movePaddle p Up   = p + 1
movePaddle p Down = p - 1
movePaddle p None = p

updateBall :: Ball -> Ball
updateBall (Ball (x, y) (vx, vy)) = Ball (x + vx, y + vy) (vx, vy)

checkCollision :: Ball -> Paddle -> Bool

modifyGame :: Game -> (Input, Input) -> Game
modifyGame game (in1, in2) = 
	where
		paddle1' = movePaddle (gPaddle1 game) in1
		paddle2' = movePaddle (gPaddle2 game) in2
		ball' = if 