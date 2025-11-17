askForWord :: Int -> IO ()
askForWord i =
    print("Write the " ++ show i ++ "th word") -- Even though print itself calls show, it calls show on the string first, not "each argument".

forLoop :: (Int -> IO ()) -> IO String -> Int -> Int -> IO ()
forLoop f g i j
    | i > j = return ()
    | otherwise = do
    f i
    x <- g
    print x
    forLoop f g (i + 1) j

takeNum :: Int -> Int
takeNum x = x

data IntAndString = IntAndString {
    num :: Int,
    str :: String
} deriving (Show)

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

returnNumAndString :: (Int -> Int) -> Int -> IntAndString
returnNumAndString f x =
    IntAndString (3) ("JOEBAL")

loop :: (Int -> IntAndList) -> ((Int -> IntAndList) -> IntAndList -> IntAndList) -> IntAndList -> Int -> Int -> IntAndList
loop f g (IntAndList ialn iall) x y
    | x > y = (IntAndList ialn iall)
    | otherwise = loop f g (g f (IntAndList ialn iall)) (x + 1) (y)

-- A monad takes in functions that return a monadic type, it has a monadic type, which then it return.

main :: IO ()
main = do
    print("How many number of words do you want?")
    print("This has been printed!")
    --forLoop askForWord getLine 0 3
    print(returnNumAndString takeNum 3)
    --print(addNum3(addNum5 3))
    print(returnWithLogging (addNum3) (returnWithLogging (addNum5) (returnWithLogging (addNum5) (ialId 3)))) -- Monad??
    print(loop addNum3 returnWithLogging (ialId 3) 3 5)
