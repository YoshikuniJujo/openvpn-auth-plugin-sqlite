module Main where

import System.IO
import System.IO.Temp

import Database.SmplstSQLite3

main :: IO ()
main = withSystemTempFile "sqlite" $ \fp h -> do
	hClose h
	mkTable fp
	adduser fp "yosh" "good-bye"
	adduser fp "josh" "hello-world"
	password fp "yosh" >>= putStrLn

mkTable :: FilePath -> IO ()
mkTable fp = withSQLite fp $ \db -> () <$
	withPrepared db "CREATE TABLE login(user PRIMARY KEY, passwd)" step

adduser :: FilePath -> String -> String -> IO ()
adduser fp u p = withSQLite fp $ \db -> (() <$) .
	withPrepared db "INSERT INTO login VALUES(:user, :passwd)" $ \sm ->
		bind sm ":user" u >> bind sm ":passwd" p >> step sm

password :: FilePath -> String -> IO String
password fp u = withSQLite fp $ \db -> (fst <$>) .
	withPrepared db "SELECT passwd FROM login where user = :user" $ \sm ->
		bind sm ":user" u >> step sm >> column sm 0
