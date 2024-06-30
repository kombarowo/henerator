module Main where

import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Time.Clock.POSIX as P
import qualified System.IO as IO
import qualified System.Random as R

main = do
    handleIO <- IO.openFile "text.txt" IO.ReadMode
    textRaw  <- IO.hGetContents handleIO
    ts       <- truncate <$> P.getPOSIXTime

    let n        = 10
    let wMap     = phrasesToMap (phrases textRaw) M.empty
    let (r, gen) = R.randomR (0, (M.size wMap) - 1) (R.mkStdGen ts) :: (Int, R.StdGen)
    let firstW   = fst $ M.elemAt r wMap
    let result   = unwords $ buildSentence [] wMap firstW n gen

    putStrLn result

{- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell#answer-4981265 -}
phrases :: String -> [String]
phrases s =
    case dropWhile isPhraseSeparator s of
        "" -> []
        s' -> w : phrases s''
            where (w, s'') = break isPhraseSeparator s'

isPhraseSeparator :: Char -> Bool
isPhraseSeparator c = c `elem` ".?!,;:-[]()\n"

phrasesToMap :: [String] -> M.Map String (M.Map String Int) -> M.Map String (M.Map String Int)
phrasesToMap [] map = map
phrasesToMap (x:xs) map
    | xs == []  = newMap
    | otherwise = phrasesToMap xs newMap
        where newMap = wordsToMap (words $ prepare x) map

wordsToMap :: [String] -> M.Map String (M.Map String Int) -> M.Map String (M.Map String Int)
wordsToMap [] map = map
wordsToMap (x:xs) map
    | xs == []  = map
    | otherwise = wordsToMap xs newMap
        where newMap = M.insertWith u x subMap map
              u      = M.unionWith (+)
              subMap = M.fromList [(head xs, 1)]

mapToWords :: M.Map String Int -> [String]
mapToWords map = M.foldrWithKey (\w n b -> (take n $ repeat w) ++ b) [] map

prepare :: String -> String
prepare s = [C.toLower c | c <- s]

buildSentence :: [String] -> M.Map String (M.Map String Int) -> String -> Int -> R.StdGen -> [String]
buildSentence ws map w n gen = do
    if length ws == n
        then ws
        else do
            let nextWs    = if M.member w map then (mapToWords $ map M.! w) else [""] -- TODO_1: Find more elegant solution to..
            let (r, gen') = R.randomR (0, (length nextWs) - 1) gen :: (Int, R.StdGen)
            let nextW     = nextWs !! (max 0 r) -- TODO_1: ..Fix "index too large"

            buildSentence (ws ++ [w]) map nextW n gen'

-- TODO_2: Update clean function to replace really ALL punctuation, also for cyrillic
