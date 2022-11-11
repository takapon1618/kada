module Ttime where

import Data.Time.LocalTime(getZonedTime,ZonedTime(zonedTimeToLocalTime),LocalTime(localDay))
import Tlibs(wake)

daylist :: [Int]
daylist = [31,28,31,30,31,30,31,31,30,31,30,31]

day :: IO String 
day = do
  da <- show <$> localDay <$> zonedTimeToLocalTime <$> getZonedTime
  return (concat$wake '-' da) 

uru :: Int -> Bool
uru y = let r1 = mod y 4 == 0
            r2 = mod y 100 == 0
            r3 = mod y 400 == 0
         in r3 || (r1 && not r2)

sepday :: String -> (Int,Int,Int)
sepday (a:b:c:d:e:f:g) = (read (a:b:c:d:[]), read (e:f:[]), read g)

dfYdays :: Int -> Int -> Int
dfYdays fy sy =
  if (fy==sy) then 0
              else (if (uru fy) then 366 else 365) + (dfYdays (fy+1) sy)

hmDays :: String -> String -> Int
hmDays fday sday =
  let (fy,fm,fd) = sepday fday
      (sy,sm,sd) = sepday sday
      fal = if (fm>1) then (sum$take (fm-1) daylist)+fd else fd
      sal = if (sm>1) then (sum$take (sm-1) daylist)+sd else sd
      fal' = if (uru fy && fm>2) then fal+1 else fal
      sal' = if (uru sy && fm>2) then sal+1 else sal
   in (dfYdays fy sy) + (sal'-fal')
     

