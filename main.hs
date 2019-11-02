import DFA(DFA)
import qualified DFA
import qualified Regex
import qualified GNFA
import System.Environment (getArgs)
import System.Random (getStdGen, StdGen)
import Text.Read (readMaybe)
import Data.List ((!!))

-- Representation of each digit as a string
baseMapping :: [String]
baseMapping = pure <$> "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- Takes a base and number to match multiples of
divDFA :: Int -> Int -> DFA Int
divDFA base number
    | number < 0                = error "Program only handles natural numbers"
    | base < 2                  = error "Base must be atleast 2"
    | base > length baseMapping = error "Base cannot be larger than character set"
    | number == 0               = DFA.minimise $ DFA.dfaRecurse True id alphabet (\b x -> b && x==0)
    | otherwise                 = DFA.minimise $ DFA.dfaRecurse 0 (==0) alphabet (\c x -> (c*base+x) `mod` number)
    where alphabet = [0..base-1]

-- Given a base, number, and amount of attempts, output a minimal regular expression
minRegex :: StdGen -> Int -> Int -> Integer -> String
minRegex _ base multiple 0 =
    Regex.renderRegex (baseMapping !!) $
    GNFA.dfaToRegexMinimal $
    divDFA base multiple
minRegex gen base multiple attempts =
    Regex.renderRegex (baseMapping !!) $
    snd $
    GNFA.dfaToRegexRandomAttempts gen attempts $
    divDFA base multiple

-- Given a number, amount of attempts, and remaining arguments, output a minimal regular expression
minRegexNA :: StdGen -> Int -> Integer -> [String] -> String
minRegexNA gen multiple attempts [] = minRegex gen 10 multiple attempts
minRegexNA gen multiple attempts (x:_) = case readMaybe x of
    Nothing -> "Third argument is not a valid number, should be base of numbering system"
    Just base
        | base < 2 ->
            "Third argument is too small, base of numbering system must be atleast 2"
        | base > length baseMapping ->
            "Third argument is too large, not enough digits are available to represent numbers in this base"
        | otherwise ->
            minRegex gen base multiple attempts

-- Given a number and remaining arguments, output minimal regular expression
minRegexN :: StdGen -> Int -> [String] -> String
minRegexN gen multiple [] = minRegexNA gen multiple 1 []
minRegexN gen multiple (x:xs) = case readMaybe x of
    Nothing -> "Second argument is not a valid number, should be number of attempts to make minimal regex"
    Just attempts
        | attempts < 0 ->
            "Second argument is too small, number of attempts must be atleast 1, or use 0 to find absolute minimum"
        | otherwise ->
            minRegexNA gen multiple attempts xs

-- Given arguments, output minimal regular expression
minRegexArgs :: StdGen -> [String] -> String
minRegexArgs _ [] = "Missing first argument, should be number to match multiples of"
minRegexArgs _ (('-':_):_) = "Program takes atleast 1 argument. First is number to match multiples of, second is number of attempts to minimise regex (default 1, use 0 to try all), third is base of numbering system (default 10)"
minRegexArgs gen (x:xs) = case readMaybe x of
    Nothing -> "First argument is not a valid number, should be number to match multiples of"
    Just number
        | number < 0 ->
            "First argument is too small, negative numbers are not supported"
        | otherwise ->
            minRegexN gen number xs

main :: IO ()
main = do
    gen <- getStdGen
    args <- getArgs
    putStrLn $ minRegexArgs gen args
