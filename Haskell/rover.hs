--
-- B1nj0y idegorepl<at>gmail.com

import Control.Monad

type Steps      = Int
type Coordinate = (Steps, Steps)
type Direction  = String
type Position   = (Coordinate, Direction)
type Movement   = String
type MoveInfo   = (Position, [Movement])

toSteps :: String -> Steps
toSteps = read

-- get the most top right coordinate
getMaxCoordinate :: String -> Coordinate
getMaxCoordinate s = (toSteps x, toSteps y)
  where [x, y] = words s

-- get the start position of every run
getStartPos :: String -> Position
getStartPos str = ((toSteps x, toSteps y), d)
  where [x, y, d] = words str

nextDirect :: Direction -> Movement -> Direction
nextDirect d "M" = d
nextDirect d "L" = case d of
                       "W" -> "S"
                       "S" -> "E"
                       "E" -> "N"
                       "N" -> "W"
nextDirect d "R" = case d of
                       "W" -> "N"
                       "N" -> "E"
                       "E" -> "S"
                       "S" -> "W"

nextCoordinate :: Coordinate -> Direction -> Movement -> Coordinate
nextCoordinate (x, y) d "M" = case d of
                                  "W" -> (x - 1, y)
                                  "E" -> (x + 1, y)
                                  "N" -> (x, y + 1)
                                  "S" -> (x, y - 1)
nextCoordinate (x, y) _ _ = (x, y)

move :: MoveInfo -> Maybe MoveInfo
move (((x, y), d), ms) = Just ((nc, nd), tail ms)
  where m  = head ms
        nd = nextDirect d m
        nc = nextCoordinate (x, y) nd m

-- check if the rover in the valid area after a series of runs
checkResult :: Coordinate -> Maybe MoveInfo -> String
checkResult mc (Just (((x, y), d), _))
    | (x, y) `elem` allPos = show x ++ " " ++ show y ++ " " ++ d
    | otherwise            = "RIP"
  where allPos = [ (x, y) | x <- [0..fst mc], y <- [0..snd mc] ]

runInstructs :: [String] -> (Int, Int) -> Maybe MoveInfo
runInstructs xs (m, n) = Just (sp, map (:[]) ins) >>= foldr (<=<) return (replicate (length ins) move)
  where sp = getStartPos $ xs !! m
        ins = xs !! n

main :: IO ()
main = do
    let instructs = [ "5 5"
                    , "1 2 N"
                    , "LMLMLMLMM"
                    , "3 3 E"
                    , "MMRMMRMRRM"
                    , "4 4 N"
                    , "MMM"
                    ]
        mc = getMaxCoordinate $ head instructs
    mapM_ print . map (checkResult mc . runInstructs instructs)
                      $ take (length instructs `div` 2) $ zip [1,3..] [2,4..]
