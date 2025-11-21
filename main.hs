import Data.Char (toUpper)

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

sortWords :: StringArr -> StringArr
sortWords (StringArr s d) = sorted
    where
    s' = map makeWordUpper s --We can't reuse parameter names in the where block because 
    count = countWords s'
    sorted = (StringArr (map fst (reverseQSByNumbers (zip s' count))) (map fst (reverseQSByNumbers (zip d count))))

main :: IO ()
main = do
    --print(returnWithLogging (addNum3) (returnWithLogging (addNum5) (returnWithLogging (addNum5) (ialId 3)))) -- Monad??
    --print(loop addNum3 returnWithLogging (ialId 3) 3 5)
    (StringArr w d) <- askAndRead ask reader "Word" "Hint" (StringArr [] []) 1 5
    print(w)
    let (StringArr nw nd) = sortWords (StringArr w d) --I'm doing let = instead of <- because it's not an IO typed variable I'm getting from the sortWords function. Using different names because binding the same name.
    print(nw)
    print(nd)
