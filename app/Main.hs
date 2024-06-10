module Main where

import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Time as Time
import qualified System.IO as IO
import qualified System.Random as R

main = do
    handleIO    <- IO.openFile "text.txt" IO.ReadMode
    textRaw     <- IO.hGetContents handleIO

    let n        = 30
    let wMap     = wordsToMap (words $ clean textRaw) M.empty
    let (r, gen) = R.randomR (0, (M.size wMap) - 1) (R.mkStdGen 1) :: (Int, R.StdGen)
    let firstW   = fst $ M.elemAt r wMap
    let result   = unwords $ buildSentence [] wMap firstW n gen

    putStrLn result

buildSentence :: [String] -> M.Map String (M.Map String Int) -> String -> Int -> R.StdGen -> [String]
buildSentence ws map w n gen = do
    if length ws == n
        then ws
        else do
            let nextWs    = if M.member w map then (mapToWords $ map M.! w) else []
            let (r, gen') = R.randomR (0, (length nextWs) - 1) gen :: (Int, R.StdGen)
            let nextW     = nextWs !! (max 0 r)

            buildSentence (ws ++ [w]) map nextW n gen'

mapToWords :: M.Map String Int -> [String]
mapToWords map = M.foldrWithKey (\w n b -> (take n $ repeat w) ++ b) [] map

clean :: String -> String
clean s = [C.toLower c | c <- s, not $ c `elem` ".?!,;:-#[]()\"\\"]

wordsToMap :: [String] -> M.Map String (M.Map String Int) -> M.Map String (M.Map String Int)
wordsToMap (x:xs) map
    | xs == []  = map
    | otherwise = wordsToMap xs newMap
    where newMap = M.insertWith u x subMap map
          u      = M.unionWith (+)
          subMap = M.fromList [(head xs, 1)]

-- TODO: Fix list range error, when text contains lonely word
-- TODO: R.mkStdGen 1 <-- replace "1" with current timestamp
-- TODO: Build wordsToMap from raw text by sentences, so it will make result lessly dumb..
-- TODO: Update clean function to replace really ALL punctuation
