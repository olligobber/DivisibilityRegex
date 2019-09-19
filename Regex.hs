{-# LANGUAGE UndecidableInstances #-}

module Regex (
    Regex,
    renderRegex
) where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Function (on)

-- from Data.Fix
newtype Fix f = Fix { unFix :: f (Fix f) }

instance Eq (f (Fix f)) => Eq (Fix f) where
    (==) = (==) `on` unFix

instance Ord (f (Fix f)) => Ord (Fix f) where
    compare = compare `on` unFix

cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata f = f . fmap (cata f) . unFix

-- Regex represented recursively using a functor fixed point
data RegexF c x = Conc [x] | Star x | Disj [x] | In (Set c) | NotIn (Set c)

type Regex c = Fix (RegexF c)

instance Functor (RegexF c) where
    fmap f (Conc list) = Conc $ fmap f list
    fmap f (Star r) = Star $ f r
    fmap f (Disj list) = Disj $ fmap f list
    fmap _ (In set) = In set
    fmap _ (NotIn set) = NotIn set

renderSet :: (Enum c, Ord c) => (c -> ShowS) -> Set c -> ShowS
renderSet rend set = case S.toAscList set of
    [] -> mempty
    [c] -> rend c
    first:list -> let
        startRender = (mempty, first, first)
        stepRender (s,a,b) c = case (a==b, succ a==b, succ b==c) of
            (_,_,True) -> (s, a, c)
            (True,_,_) -> (s <> rend a, c, c)
            (_,True,_) -> (s <> rend a <> rend b, c, c)
            _ -> (s <> rend a <> showString "-" <> rend b, c, c)
        endRender (s,a,b) = case (a==b, succ a==b) of
            (True,_) -> s <> rend a
            (_,True) -> s <> rend a <> rend b
            _ -> s <> rend a <> showString "-" <> rend b
        in endRender $ foldl stepRender startRender list

renderRegexF :: (Enum c, Ord c) => (c -> ShowS) -> RegexF c ShowS -> ShowS
renderRegexF _ (Conc list) = mconcat list
renderRegexF _ (Star s) = s <> showString "*"
renderRegexF _ (Disj list) = showString "(" <> mconcat (L.intersperse (showString "|") list) <> showString ")"
renderRegexF rend (In set) = case S.size set of
    0 -> error "Cannot render regex that matches nothing"
    1 -> renderSet rend set
    _ -> showString "[" <> renderSet rend set <> showString "]"
renderRegexF rend (NotIn set)
    | S.null set    = showString "."
    | otherwise     = showString "[^" <> renderSet rend set <> showString "]"

renderRegex :: (Enum c, Ord c) => (c -> String) -> Regex c -> String
renderRegex rend regex = cata (renderRegexF $ showString . rend) regex ""
