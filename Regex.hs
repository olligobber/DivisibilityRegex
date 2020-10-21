{-# LANGUAGE UndecidableInstances, LambdaCase #-}

module Regex (
    Regex,
    renderRegex,
    dot,
    epsilon,
    empty,
    char,
    conc,
    union,
    star,
    size,
    smallest
) where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Function (on)

-- from Data.Fix
newtype Fix f = Fix { unFix :: f (Fix f) }

-- debug
instance Show (f (Fix f)) => Show (Fix f) where
    showsPrec n x = showParen (n > 10) $ \s ->
        "Fix " ++ showsPrec 11 (unFix x) s

cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata f = f . fmap (cata f) . unFix

-- Regex represented recursively using a functor fixed point
type Regex c = Fix (RegexF c)

data RegexF c x =
    Conc [x] | -- Concatenate a list of regex
    Star x | -- Star closure of regex
    Disj (SingleChar c) [x] -- Union of a single character with a list of regex
    deriving (Show) -- debug

-- Match a character that is either in or not in a set
data SingleChar c = In (Set c) | NotIn (Set c)
    deriving (Show) -- debug

instance Functor (RegexF c) where
    fmap f (Conc list) = Conc $ fmap f list
    fmap f (Star regex) = Star $ f regex
    fmap f (Disj singlechar list) = Disj singlechar $ fmap f list

-- Render regex
renderRegex :: (Enum c, Ord c) => (c -> String) -> Regex c -> String
renderRegex rend regex = ($ "") . snd $ cata (renderRegexF $ showString . rend) regex

-- Render a series of characters using dashes for ranges
renderSet :: (Enum c, Ord c) => (c -> ShowS) -> Set c -> ShowS
renderSet rend set = case S.toAscList set of
    [] -> mempty
    [c] -> rend c
    first:list -> let
        startRender = (mempty, first, first)
        stepRender (string,lower,upper) next =
            case (lower==upper, succ lower==upper, succ upper==next) of
                (_,_,True) -> (string, lower, next)
                (True,_,_) -> (string <> rend lower, next, next)
                (_,True,_) -> (string <> rend lower <> rend upper, next, next)
                _ -> (string <> rend lower <> showString "-" <> rend upper, next, next)
        endRender (string,lower,upper) = case (lower==upper, succ lower==upper) of
            (True,_) -> string <> rend lower
            (_,True) -> string <> rend lower <> rend upper
            _ -> string <> rend lower <> showString "-" <> rend upper
        in endRender $ foldl stepRender startRender list

renderSingle :: (Enum c, Ord c) => (c -> ShowS) -> SingleChar c -> Maybe ShowS
renderSingle rend (In set) = case S.size set of
    0 -> Nothing
    1 -> Just $ renderSet rend set
    _ -> Just $ showString "[" <> renderSet rend set <> showString "]"
renderSingle rend (NotIn set)
    | S.null set    = Just $ showString "."
    | otherwise     = Just $ showString "[^" <> renderSet rend set <> showString "]"

-- Render regex with and without brackets forced
renderRegexF :: (Enum c, Ord c) => (c -> ShowS) -> RegexF c (ShowS, ShowS) -> (ShowS, ShowS)
renderRegexF _ (Conc list) = (showString "(" <> mconcat (snd <$> list) <> showString ")", mconcat $ snd <$> list)
renderRegexF _ (Star (string,_)) = (showString "(" <> string <> showString "*)", string <> showString "*")
renderRegexF rend (Disj singlechar list) = (\x -> (x,x)) $ case (renderSingle rend singlechar, list) of
    (Nothing, []) -> error "Cannot render regex that matches nothing"
    (Nothing, strings) -> disjstring strings
    (Just string, []) -> string
    (Just string, strings) -> disjstring $ (string,string):strings
    where
        disjstring liststring = showString "(" <>
            mconcat (L.intersperse (showString "|") $ snd <$> liststring) <> showString ")"

-- Matches any character
dot :: Regex c
dot = Fix $ Disj (NotIn S.empty) []

-- Matches empty string
epsilon :: Regex c
epsilon = Fix $ Conc []

-- Matches no strings
empty :: Regex c
empty = Fix $ Disj (In S.empty) []

-- Matches one character
char :: c -> Regex c
char c = Fix $ Disj (In $ S.singleton c) []

-- Checks if regex matches nothing
isEmpty :: Regex c -> Bool
isEmpty (Fix (Disj (In set) [])) = S.null set
isEmpty _ = False

-- Check if regex is epsilon
isEpsilon :: Regex c -> Bool
isEpsilon (Fix (Conc [])) = True
isEpsilon _ = False

-- Concatenate regex
conc :: Regex c -> Regex c -> Regex c
conc _ regex | isEmpty regex = empty
conc regex _ | isEmpty regex = empty
conc regex1 regex2
    | isEpsilon regex1 = regex2
    | isEpsilon regex2 = regex1
conc (Fix (Conc list1)) (Fix (Conc list2)) = Fix $ Conc $ list1 ++ list2
conc (Fix (Conc list)) regex = Fix $ Conc $ list ++ [regex]
conc regex (Fix (Conc list)) = Fix $ Conc $ regex:list
conc regex1 regex2 = Fix $ Conc [regex1, regex2]

-- Union two single characters
unionSingle :: Ord c => SingleChar c -> SingleChar c -> SingleChar c
unionSingle (In set1) (In set2) = In $ set1 <> set2
unionSingle (In set1) (NotIn set2) = NotIn $ set2 `S.difference` set1
unionSingle (NotIn set1) (In set2) = NotIn $ set1 `S.difference` set2
unionSingle (NotIn set1) (NotIn set2) = NotIn $ set1 `S.intersection` set2

-- Union regex
union :: Ord c => Regex c -> Regex c -> Regex c
union regex1 regex2
    | isEmpty regex1 = regex2
    | isEmpty regex2 = regex1
union (Fix (Disj singlechar1 list1)) (Fix (Disj singlechar2 list2)) =
    Fix $ Disj (singlechar1 `unionSingle` singlechar2) $ list1 ++ list2
union (Fix (Disj singlechar list)) regex = Fix $ Disj singlechar $ regex:list
union regex (Fix (Disj singlechar list)) = Fix $ Disj singlechar $ regex:list
union regex1 regex2 = Fix $ Disj (In S.empty) [regex1, regex2]

-- Star closure regex
star :: Regex c -> Regex c
star regex
    | isEmpty regex = epsilon
    | isEpsilon regex = epsilon
star (Fix (Star regex)) = Fix $ Star regex
star regex = Fix $ Star regex

-- Get the size of a regex
size :: Regex c -> Integer
size = cata $ \case
    Conc list -> sum list + 1
    Star i -> i+1
    Disj (In set) list -> toInteger (S.size set) + sum list + 1
    Disj (NotIn set) list -> toInteger (S.size set) + sum list + 1

-- Get the smallest of two regex
smallest :: Regex c -> Regex c -> Regex c
smallest r1 r2 = if ((<) `on` size) r1 r2 then r1 else r2
