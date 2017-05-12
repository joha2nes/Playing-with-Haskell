-- file.hs

import System.IO
import Data.Char
import System.Directory

main = do
	(tempName, tempHandle) <- openTempFile "." "temp"
	getLine
	hClose tempHandle
	removeFile tempName