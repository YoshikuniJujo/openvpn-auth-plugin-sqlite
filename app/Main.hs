{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad

import Data.Maybe
import Data.List
import Data.Bits
import Data.Word
import Data.Char
import Data.ByteString (ByteString)

import System.IO
import System.Environment
import System.Exit
import System.Posix.User
import System.Posix.Files
import Crypto.Hash

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = do
	hPutStrLn stderr "HELOO HELLO BOOOOOOOOOOOOOOOOO"
	fp : _as <- getArgs
	fs <- getFileStatus fp
	hPutStrLn stderr fp
	hPutStrLn stderr . show $ fileMode fs
	hPutStrLn stderr . show =<< getUserEntryForID (fileOwner fs)
	h <- openFile fp ReadMode
	un <- BSC.hGetLine h
	p <- BSC.hGetLine h
	when (un /= "yosh") exitFailure
--	u <- (<> "\n") . BSC.pack <$> getEffectiveUserName
	h0 <- hello
	let	h1 = BA.convert $ sha512 p
	when (h1 /= h0) $ do
		writeFile "/home/tatsuya/openvpn_auth.log" $
			show h0 <> "\n" <> show h1 <> "\n"
		exitFailure

	BS.writeFile "/home/tatsuya/openvpn_auth.log" $
		un <> "\n" <> p <> "\n"

sha512 :: ByteString -> Digest SHA512
sha512 = hash

hexDigits :: [Char]
hexDigits = "0123456789abcdef"

hexToNum1 :: Char -> Word8
hexToNum1 = fromIntegral . fromJust . (`elemIndex` hexDigits)

readHex :: Char -> Char -> Word8
readHex a b = (hexToNum1 a `shift` 4) .|. hexToNum1 b

readHash :: String -> ByteString
readHash = BS.pack . rh
	where
	rh (a : b : cs) = readHex a b : rh cs
	rh "" = []
	rh _ = error "bad"

hello :: IO ByteString
hello = (readHash . takeWhile (not . isSpace) <$>) $ do
	h <- openFile "/etc/openvpn/password-file.txt" ReadMode
	fp <- hGetLine h
	readFile fp
