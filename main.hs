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

data PlacedWordState = PlacedWordState {
    word :: String,
    isAcross :: Bool
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

findCommonLetter :: String -> String -> Maybe (Char, Int, Int) -- The possible common character, and the positions of each in word of said character.
findCommonLetter placed unplaced =
    let positions = [(c, i, j) | (i, c) <- zip [0..] placed, elem c unplaced, let j = findIndex c unplaced] -- elem is a filter that will only keep c if it exists in the other array (which is a string).
    in case positions of
        [] -> Nothing --If the list of positions is empty, return nothing.
        (x:_) -> Just x --If the list of positions has atleast one element, return the first one.
    where
        findIndex :: Char -> String -> Int
        findIndex char s = head [i | (i, c) <- zip[0..] s, char == c]

loopThroughUnplacedWords :: [String] -> [String] -> Int -> Int -> Int -> Maybe (Char, Int, Int, Int) -- This returns the character, the unplaced index, the positions of the letter in the first and the second words.
loopThroughUnplacedWords placedWords unplacedWords unplacedWordsToBeCompared placedIndex unplacedIndex
    | unplacedIndex >= unplacedWordsToBeCompared = Nothing
    | otherwise =
        case findCommonLetter (placedWords !! placedIndex) (unplacedWords !! unplacedIndex) of
        Nothing -> loopThroughUnplacedWords placedWords unplacedWords unplacedWordsToBeCompared placedIndex (unplacedIndex + 1)
        Just (char, posInFirstWord, posInSecondWord) -> Just (char, unplacedIndex, posInFirstWord, posInSecondWord)

loopThroughPlacedWords :: [String] -> [String] -> Int -> Int -> Maybe (Char, Int, Int, Int, Int) -- This returns the character, the placed index, the unplaced index, the position in the first and the second words. (placed and unplaced respectively).
loopThroughPlacedWords placedWords unplacedWords placedIndex placedWordsToBeCompared
    | placedIndex >= placedWordsToBeCompared = Nothing
    | otherwise =
        let uwtbc = length unplacedWords
        in case loopThroughUnplacedWords placedWords unplacedWords uwtbc placedIndex 0 of
        Nothing -> loopThroughPlacedWords placedWords unplacedWords (placedIndex + 1) placedWordsToBeCompared
        Just (char, ui, posInFirstWord, posInSecondWord) -> Just (char, placedIndex, ui, posInFirstWord, posInSecondWord)

findCommonLetterBetweenWords :: [String] -> [String] -> Maybe (Char, Int, Int, Int, Int) --Returns the character in common, the offset from the placed word, the offset from the unplaced word, the position inside the placed word and the position inside the unplaced word.
findCommonLetterBetweenWords placedWords unplacedWords =
    let pwtbc = length placedWords
    in case loopThroughPlacedWords placedWords unplacedWords 0 pwtbc of
    Nothing -> Nothing
    Just (char, placedIndex, ui, posInFirstWord, posInSecondWord) -> Just (char, placedIndex, ui, posInFirstWord, posInSecondWord)

gridWords :: (StringArr, [Int]) -> IO ()
gridWords ((StringArr s d), numList) = do
    --I have an incorrect formula. If the First word intersects with the smallest word, and the second word intersects with the smallest word also, the ACROSS, will not have enough tiles!
    --(1): Let's assume that we always start with across rather than down.
    --Worst case scenario: For list of Words W1, W2, W3, W4, W5 .. Wn with respective length of words |W1|, |W2|, |W3|, |W4|, |W5| .. |Wn| with the assumption (1).
    --The maximum size across (Sa) for the grid should be: |W1| + 2 * (|W3| - 1) + 2 * (W(k * 2 - 1) - 1) where k = 2 ... n/2
    --The maximum size vertically (Sv) for the grid should be: |W2| + 2 * (|W4| - 1) + (W(k * 2) - 1) where k = 2 ... n/2
    --The position i, j of the first word then should be i = ((Sa - |W1|) / 2), j = ((Sa - 1) / 2), where i ranges from 0 to (Sa - 1) and j ranges from 0 to (Sv - 1).
    let numIndex = zip [0..] numList
        evenIndices = [e | (i, e) <- numIndex, mod i 2 == 0]
        oddIndices = [o | (i, o) <- numIndex, mod i 2 /= 0]
        sizeAcross =
            let (h: ts) = evenIndices
                processedTailEven = map (\x -> (x - 1) * 2) ts
            in (sum (h : processedTailEven))
        sizeDown = (1 + (sum (map (\x -> (x - 1) * 2) oddIndices)))
        firstWordIndexI = (div (sizeAcross - head numList) 2) + 1 -- + 1 for both indices cause array indics in Haskell start from 1.
        firstWordIndexJ = (div (sizeDown - 1) 2) + 1
        initialGrid = makeGrid sizeDown sizeAcross
        grid' = (setCells initialGrid (placeWord (s !! 0) (firstWordIndexJ, firstWordIndexI) True))
        placedWords = [s !! 0]
        unplacedWords = tail s
        grid'' = case findCommonLetterBetweenWords placedWords unplacedWords of
            Nothing -> grid'
            Just (commonCharacter, placedWordOffset, unplacedWordOffset, placedIndex, unplacedIndex) -> (setCells grid' (placeWord (unplacedWords !! unplacedWordOffset) ((firstWordIndexJ - unplacedIndex), (firstWordIndexI + placedIndex)) False))

    --in case findCommonLetterBetweenWords 
    printGrid grid''

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
