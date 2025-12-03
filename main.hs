import Data.Char (toUpper)
import Data.Array

ask :: Int -> String -> IO ()
ask i s =
    print(s ++ " " ++ show i ++ " is: ") -- Even though print itself calls show, it calls show on the string first, not "each argument".

data IntAndList = IntAndList {
    nue :: Int,
    lst :: [Int]
} deriving (Show)

returnNumbers :: Int -> Int
returnNumbers x = x

addNum3 :: Int -> IntAndList
addNum3 x = IntAndList (x + 3) [(x + 3)]

addNum5 :: Int -> IntAndList
addNum5 x = IntAndList (x + 5) [(x + 5)]

ialId :: Int -> IntAndList
ialId x =
        IntAndList (x)
                   ([x])

returnWithLogging :: (Int -> IntAndList) -> IntAndList -> IntAndList
returnWithLogging f (IntAndList ialn iall) = temp
    where
        IntAndList j1 j2 = f ialn
        temp = IntAndList j1 (iall ++ j2)

loop :: (Int -> IntAndList) -> ((Int -> IntAndList) -> IntAndList -> IntAndList) -> IntAndList -> Int -> Int -> IntAndList
loop f g (IntAndList ialn iall) x y
    | x > y = (IntAndList ialn iall)
    | otherwise = loop f g (g f (IntAndList ialn iall)) (x + 1) (y)

-- A monad takes in functions that return a monadic type, it has a monadic type, which then it return.

data StringArr = StringArr {
    words :: [String],
    explanations :: [String]
} deriving (Show)

reader :: () -> IO (StringArr)
reader () = do
    line <- getLine
    print(line)
    return (StringArr [line] [])

askAndRead :: (Int -> String -> IO ()) -> (() -> IO (StringArr)) -> String -> String -> StringArr -> Int -> Int -> IO (StringArr)
askAndRead f g word description (StringArr w e) i j
    | i > j = return (StringArr w e)
    | otherwise = do
        f i word
        (StringArr w1 w2) <- g ()
        f i description
        (StringArr e1 e2) <- g ()
        askAndRead f g word description(StringArr (w ++ w1) (e ++ e1)) (i + 1) j

makeWordUpper :: String -> String
makeWordUpper s = [toUpper c | c <- s]

countLetters :: String -> Int
countLetters s = length [c | c <- s, isLetter c] --List comprehension isLetter is a filter/guard/condition that will only give to c if it passes. String is a list of chars, so I am passing each char to c through c <- s
    where
    isLetter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

countWords :: [String] -> [Int]
countWords s = map countLetters s

reverseQSByNumbers :: [(String, Int)] -> [(String, Int)]
reverseQSByNumbers [] = []
reverseQSByNumbers (p:ps) =
    let smaller = [x | x <- ps, snd x <= snd p] --If second element (Int) of the rest of the pattern matched tail list is smaller than the head, it is added to the list.
        larger = [x | x <- ps, snd x > snd p] --If second element (Int) of the rest of the pattern matched tail list is bigger than the head, it is added to the list.
    in reverseQSByNumbers larger ++ [p] ++ reverseQSByNumbers smaller --in is a part of the let in expression where the let bindings are used in an expression.

sortWords :: StringArr -> (StringArr, [Int])
sortWords (StringArr s d) = sorted
    where
    s' = map makeWordUpper s --We can't reuse parameter names in the where block because 
    count = countWords s'
    sorted = ((StringArr (map fst (reverseQSByNumbers (zip s' count))) (map fst (reverseQSByNumbers (zip d count)))), count)

type Grid = Array (Int, Int) Char

makeGrid :: Int -> Int -> Grid
makeGrid rows columns =
    array ((1, 1), (rows, columns))
          [((i, j), '_') | i <- [1..rows], j <- [1..columns]]

setCell :: Grid -> (Int, Int) -> Char -> Grid
setCell grid pos char = grid // [(pos, char)]

printGrid :: Grid -> IO ()
printGrid grid =
  let ((_, _), (maxR, maxC)) = bounds grid
  in mapM_ putStrLn 
       [[grid ! (r,c) | c <- [1..maxC]] | r <- [1..maxR]]

setCells :: Grid -> [((Int, Int), Char)] -> Grid
setCells grid updates = grid // updates

placeWord :: String -> (Int, Int) -> Bool -> [((Int, Int), Char)]
placeWord word (row, col) isAcross =
  if isAcross
    then [((row, col + i), c) | (i, c) <- zip [0..] word]
    else [((row + i, col), c) | (i, c) <- zip [0..] word]

gridWords :: (StringArr, [Int]) -> IO ()
gridWords ((StringArr s d), numList) = do
    --(1): Let's assume that we always start with across rather than down.
    --Worst case scenario: For list of Words W1, W2, W3, W4, W5 .. Wn with respective length of words |W1|, |W2|, |W3|, |W4|, |W5| .. |Wn| with the assumption (1).
    --The maximum size across (Sa) for the grid should be: |W1| + 2 * (|W3| - 1) + 2 * (W(k * 2 - 1) - 1) where k = 2 ... n/2
    --The maximum size vertically (Sv) for the grid should be: |W2| + 2 * (|W4| - 1) + (W(k * 2) - 1) where k = 2 ... n/2
    --The position i, j of the first word then should be i = ((Sa - |W1|) / 2), j = ((Sa - 1) / 2), where i ranges from 0 to (Sa - 1) and j ranges from 0 to (Sv - 1).
    let numIndex = zip [0..] numList
    let evenIndices = [e | (i, e) <- numIndex, mod i 2 == 0]
    let oddIndices = [o | (i, o) <- numIndex, mod i 2 /= 0]
    let sizeAcross =
            let (h: ts) = evenIndices
                processedTailEven = map (\x -> (x - 1) * 2) ts
            in (sum (h : processedTailEven))
    let sizeDown = (1 + (sum (map (\x -> (x - 1) * 2) oddIndices)))
    let firstWordIndexI = (div (sizeAcross - head numList) 2) + 1 -- + 1 for both indices cause array indics in Haskell start from 1.
    let firstWordIndexJ = (div (sizeDown - 1) 2) + 1
    let initialGrid = makeGrid sizeAcross sizeDown
    let grid' = (setCells initialGrid (placeWord (s !! 0) (firstWordIndexJ, firstWordIndexI) True))
    printGrid grid'
    print(firstWordIndexI)

main :: IO ()
main = do
    --print(returnWithLogging (addNum3) (returnWithLogging (addNum5) (returnWithLogging (addNum5) (ialId 3)))) -- Monad??
    --print(loop addNum3 returnWithLogging (ialId 3) 3 5)
    (StringArr w d) <- askAndRead ask reader "Word" "Hint" (StringArr [] []) 1 3
    print(w)
    let ((StringArr nw nd), lengthList) = sortWords (StringArr w d) --I'm doing let = instead of <- because it's not an IO typed variable I'm getting from the sortWords function. Using different names because binding the same name.
    print(nw)
    print(nd)
    gridWords((StringArr nw nd), lengthList)
    --printGrid (makeGrid 2 2)
