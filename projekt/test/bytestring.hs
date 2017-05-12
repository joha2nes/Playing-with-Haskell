import System.Environment
import qualified Data.ByteString as B'
import qualified Data.ByteString.Lazy as B


--main = putStr "Hello " >> putStrLn "World"
main = do 
	putStr "Hello "
	putStr "World"

{-
main = do
	(fromFile:toFile:_) <- getArgs
	copyFiles fromFile toFile

copyFiles :: FilePath -> FilePath -> IO ()
copyFiles source dest = do
	contents <- B.readFile source
	B.writeFile dest contents
-}