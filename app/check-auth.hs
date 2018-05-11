import Control.Exception
import System.IO
import System.Environment
import System.Process
import System.IO.Temp

main :: IO ()
main = do
	cmd : u : _ <- getArgs
	withSystemTempFile "auth" $ \fp h -> do
		hPutStrLn h u
		withEcho False getLine >>= hPutStrLn h
		hClose h
		(_, _, _, ph) <- createProcess $ proc "stack" ["exec", cmd, fp]
		waitForProcess ph >>= print

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
	old <- hGetEcho stdin
	bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
