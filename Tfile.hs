module Tfile where
 
import System.IO (IOMode (ReadMode,WriteMode), openFile, hClose, hPutStr, hGetContents, hSetEncoding, utf8)
import System.Directory(doesFileExist)

filepath :: String
filepath = "./kada.txt"

fileAru :: IO Bool
fileAru = doesFileExist filepath

fileRead :: IO String 
fileRead = do
  h <- openFile filepath ReadMode
  hSetEncoding h utf8
  c <- hGetContents h
  putStr c
  hClose h
  return c

fileWrite :: String -> IO ()
fileWrite s = do
  h <- openFile filepath WriteMode
  hSetEncoding h utf8
  hPutStr h s
  hClose h
