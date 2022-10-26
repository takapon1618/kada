import System.IO (IOMode (ReadMode,WriteMode), openFile, hClose, hPutStr, hGetContents, hSetEncoding, utf8,hSetBuffering,stdout,BufferMode(NoBuffering))
import Data.Time.LocalTime(getZonedTime,ZonedTime(zonedTimeToLocalTime),LocalTime(localDay))
import Control.Monad
import System.Directory(doesFileExist)
import Data.Char(isDigit)

data Td = N Int | L Char | LN Char Int | S String | Ot deriving (Eq)

instance Show Td where
  show (N i) = show i
  show (L c) = [c]
  show (LN c i) = [c]++(show i)
  show (S s) = s
  show Ot = "Ot"

filepath :: String
filepath = "./kada.txt"

daylist :: [Int]
daylist = [31,28,31,30,31,30,31,31,30,31,30,31]

kamokuEn = ["ma","na","en","hi","ge","ph","bi","te","li","mu","ar","pe"]
kamokuJp = ["数学","国語","英語","歴史","地理","物理","生物","技術","家庭","音楽","美術","体育"]

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

showKada :: String -> IO ()
showKada "" = return ()
showKada c = do
  let cs = lines c
  mapM_ showka cs

showka :: String -> IO ()
showka ka = do
  today <- day
  let kas = wake ';' ka
      ik = (length kas==3)
      kas1 = head kas
      kamoku = showKamoku (take 2 kas1)
      tei = showTei (head$drop 2 kas1)
      nama = drop 3 kas1
      kigen = kas!!1
      zenka = map show (conTodo (kas!!2))
      kas3 = if ik then "やった～!" else kas!!3
      imaka = if ik then [] else wake ',' kas3
      lzen = length zenka
      lima = length imaka
      hmd = hmDays today kigen
      lzeni = fromIntegral lzen
      limai = fromIntegral lima
      par = floor$(lzeni-limai)/lzeni*100
      ba = div par 10
      bar = "|" ++ (concat$replicate ba "=>") 
                ++ (concat$replicate (10-ba) "--") ++ "|"
      hyouzi = "\n" ++ kamoku ++ ":" ++ tei ++ ":" ++ nama ++"\n" ++
               kas3 ++ "\n" ++ bar ++" 期限:" ++ kigen ++ " -- あと"++
               (show hmd) ++ "日"++" (" ++ (show par) ++ "% 完了)"
  putStrLn hyouzi
      

showKamoku :: String -> String
showKamoku e = kamokuJp !! (getIndex e kamokuEn) 

showTei :: Char -> String
showTei t = case t of
              'w' -> "ワーク"
              'p' -> "プリント"
              'n' -> "ノート"
              _   -> "なんだそれ"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  e <- doesFileExist filepath
  c <- if e then fileRead else fileWrite "" >> return ""
  showKada c
  s <- sousa
  putStrLn s
  iq <- case s of
    "a" -> do
      comM <- kamoku >>= syos >>= nama >>= hiz >>= page
      let iq' = comM==Nothing
      if iq' then return iq' else do
        let Just com = comM
            nc = tui c com
        fileWrite nc
        putStrLn "追加されたよ！"
        putStr nc
        return iq'
    "d" -> do
      comM <- kamoku >>= syos >>= nama
      let iq' = comM==Nothing
      if iq' then return iq' else do
        let Just com = comM
            nc = kesi c com
        if (nc==c) then do
          putStrLn "そんな課題ないよ～"
          return iq'
                   else do
          fileWrite nc
          putStrLn "消去されたよ！！！"
          putStr nc
          return iq'
    "f" -> do
       let allKesu = putStrLn "『all』って入力すると全部消せるよ！！"
           hyouji = showKada c
       com' <- kamoku >>= syos >>= nama
       if (com'==Nothing) then return () else allKesu >> hyouji
       comM <- page com'
       let iq' = comM==Nothing
       if iq' then return iq' else do
        let Just com = comM
            nc = kan c com
        fileWrite nc
        putStrLn "変更したよおお！！ おめでと～～☆"
        putStr nc
        return iq'
    "c" -> do
       putStrLn "宿題の期限変更するよ！！"
       let hyouji = showKada c
       com' <- kamoku >>= syos >>= nama
       if (com'==Nothing) then return () else hyouji
       comM <- hiz com'
       let iq' = comM==Nothing
       if iq' then return iq' else do
         let Just com = comM
             nc = hen c com
         fileWrite nc
         putStrLn "期限を変更したよ！！"
         putStrLn "がんばってね！！"
         getLine
         putStr nc
         return iq'
    "q" -> do
       putStrLn "お疲れ様でしたあああああ"
       return True
  if iq then return () else main

tui :: String -> String -> String
tui c f = 
  let sb = head$wake ';' f
      ln = lines c
      is = elem sb (map (head . wake ';') ln)
   in if is then c 
            else let pgs = last$wake ';' f
                  in unlines$ln++[f++(joinChar ',' (map show (conTodo pgs)))]

kesi :: String -> String -> String
kesi c f =
  let len = length f
      cs = lines c
      sln = map (take len) cs
      is = elem f sln
      id = if is then getIndex f sln else (-1)
   in if is then unlines$kesu id cs else c

kesu :: Int -> [a] -> [a]
kesu i cs = take i cs ++ drop (i+1) cs 

hen :: String -> String -> String
hen c f =
  let fw = wake ';' f
      sb = head fw
      hi = last fw
      cs = lines c
      sln = map (head . (wake ';')) cs
      is = elem sb sln
      i = if is then getIndex sb sln else (-1)
   in if is then unlines$hee i hi cs else c 

kan :: String -> String -> String
kan c f =
  let fw = wake ';' f
      sb = head fw
      fl = last fw
      cs = lines c
      sln = map (head . (wake ';')) cs
      is = elem sb sln
      i = if is then getIndex sb sln else (-1)
   in if is then unlines$kae i fl cs else c


hee :: Int -> String -> [String] -> [String]
hee i hi cs =
  let od = cs!!i
      wod = wake ';' od
      wpa = drop 2 wod
      nd = (head wod)++";"++hi++";"++(joinChar ';' wpa)
   in take i cs ++ [nd] ++ drop (i+1) cs

kae :: Int -> String -> [String] -> [String]
kae i fl cs =
  let od = cs!!i
      wod = wake ';' od
      ol = last wod
      opgs = wake ',' ol
      fpgs = if (fl=="all") then opgs else map show (conTodo fl)
      nd = joinChar ';' ((init wod) ++ [joinChar ',' (nuku fpgs opgs)])
   in take i cs ++ [nd] ++ drop (i+1) cs

nuku :: Eq a => [a] -> [a] -> [a]
nuku [] o = o
nuku (x:xs) o =
  let is = elem x o
      id = if is then getIndex x o else (-1)
      no = if is then kesu id o else o
   in nuku xs no
      


getIndex :: Eq a => a -> [a] -> Int
getIndex _ [] = 0 
getIndex t (k:ks) = if(t==k) then 0 else 1+(getIndex t ks)
      

sousa :: IO String 
sousa = do
  putStrLn "操作を選択してください"
  putStrLn "a: 追加, d: 消去, c: 変更, f: 完了, q: 終了"
  putStr "> "
  d <- getLine
  let b = elem d ["a","c","d","f","q"]
  if b then return d else do
    putStrLn "ちが～う！ そうじゃな～い！"
    sousa

kamoku :: IO (Maybe String)
kamoku = do
  putStrLn "教科を選択してください"
  putStrLn "ma: 数学, na: 国語, en: 英語, hi: 歴史, ge: 地理, ph: 物理"
  putStrLn "bi: 生物, te: 技術, li: 家庭, mu: 音楽, ar: 美術, pe: 体育"
  putStr "> "
  d <- getLine
  let b = elem d ["ma","na","en","hi","ge","ph","bi","te","li","mu","ar","pe"]
  if b then return (Just d) else
    if (d=="q") then return Nothing else do
      putStrLn "打ち間違ってない？"
      kamoku

syos :: Maybe String -> IO (Maybe String)
syos Nothing = return Nothing
syos (Just s) = do
  putStrLn "提出する物を選択してください"
  putStrLn "w: ワーク, p: プリント, n: ノート"
  putStr "> "
  d <- getLine
  let b = elem d ["w","p","n"]
  if b then return (Just (s++d)) else do
    if (d=="q") then return Nothing else do
      putStrLn "打ち間違ってない？"
      syos (Just s) 

nama :: Maybe String -> IO (Maybe String)
nama Nothing = return Nothing
nama (Just s)= do
  putStrLn "名前ヲ入力セヨ"
  putStr "> "
  d <- getLine
  if (d=="q") then return Nothing else return (Just (s++d++";"))

hiz :: Maybe String -> IO (Maybe String)
hiz Nothing = return Nothing
hiz (Just s) = do
  putStrLn "提出期限を入力してください"
  putStr "> "
  d <- getLine
  if (d=="q") then return Nothing else return (Just (s++d++";"))

page :: Maybe String -> IO (Maybe String)
page Nothing = return Nothing
page (Just s)= do
  putStrLn "課題のページを入力してください"
  putStr "> "
  d <- getLine
  if (d=="q") then return Nothing else return (Just (s++d++";"))

toList :: (Enum a,Ord a) => a -> a -> [a]
toList a b = if(a==b) then [a] else
             if (a<b) then [a..b] else [b..a]

isNum :: String -> Bool
isNum [] = True
isNum (x:xs) = (isDigit x) && (isNum xs)

isChar :: String -> String -> Bool
isChar [] _ = True 
isChar (x:xs) str = (elem x str) && (isChar xs str)

isStr :: String -> Bool
isStr [] = True
isStr (x:xs) = (not$isDigit x) && (isStr xs)


isTodo :: String -> Bool
isTodo s = 
  let tdl = wake ',' s
   in and$map isto tdl

conTodo :: String -> [Td]
conTodo s =
  let tdl = wake ',' s
   in if(isTodo s) then concat$map conto tdl else []

conto :: String -> [Td]
conto s =
  if(elem '-' s) then
      let (a:b:_) =  wake '-' s
       in cvTdList (cvTd a) (cvTd b) 
                 else [cvTd s]

cvTdList :: Td -> Td -> [Td]
cvTdList (N a) (N b) = map (cvTd . show) (toList a b) 
cvTdList (L a) (L b) = map (cvTd . (flip (:) [])) (toList a b) 
cvTdList (LN c a) (LN _ b) = map (cvTd . ((:) c) . show) (toList a b) 
cvTdList _ _ = []
  
isto :: String -> Bool
isto s =
  if(elem '-' s) then 
      let (a:b:c) = wake '-' s
       in if(c==[]) then canListTd (cvTd a) (cvTd b) 
                    else False
                 else if (cvTd s==Ot) then False else True

canListTd :: Td -> Td -> Bool
canListTd (N _) (N _) = True
canListTd (L _) (L _) = True
canListTd (LN a _) (LN b _) = a==b
canListTd _ _ = False

cvTd :: String -> Td 
cvTd [] = Ot
cvTd s@(h:t)
  | (isDigit h) && (isNum t)     = N (read s)
  | t==[] && (not$isDigit h)     = L h
  | (not$isDigit h) && (isNum t) = LN h (read t)
  | isStr s                      = S s
  | otherwise                    = Ot 

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

day :: IO String 
day = do
  da <- show <$> localDay <$> zonedTimeToLocalTime <$> getZonedTime
  return (concat$wake '-' da) 

wake :: Char -> String -> [String]
wake ch [x]    = if(x==ch) then [[]] else [[x]]
wake ch (x:xs) = if(x==ch) then [[]]++(hw:tw) 
                           else [x:hw] ++ tw 
                              where (hw:tw) = wake ch xs

tuna :: Char -> [String] -> String
tuna _ [x] = x
tuna ch (x:xs) = x++ [ch]++ (tuna ch xs)

joinChar :: Char -> [String] -> String
joinChar _ [] = []
joinChar _ [x] = x
joinChar ch (x:xs) = x++[ch]++(joinChar ch xs)

---
