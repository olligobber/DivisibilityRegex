module GNFA (
    dfaToRegexByOrder
    -- TODO
) where

import DFA (DFA)
import qualified DFA
import Regex (Regex)
import qualified Regex
import Data.Array (Array, (!), (//))
import qualified Data.Array as A
import qualified Data.Set as S

-- States in a GNFA are represented by integers 1..n+2, where 1 is the start state and 2 is the accept state
newtype GNFA c = GNFA { transitions :: Array (Integer, Integer) (Regex c) }

-- convert a DFA to a GNFA
fromDFA :: Ord c => DFA c -> GNFA c
fromDFA dfa = foldl addTransition baseGNFA allTransitions where
    firstgnfa = A.listArray ((1,1), (DFA.numstates dfa+2, DFA.numstates dfa+2)) (repeat Regex.empty)
    withstart = firstgnfa // [((1,3), Regex.epsilon)]
    baseGNFA = GNFA $ withstart //
        ((\x -> ((x+2,2),Regex.epsilon)) <$> filter (DFA.accepts dfa) [1..DFA.numstates dfa])
    allTransitions = (\start letter -> (start+2, letter, 2+DFA.move dfa start letter))
        <$> [1..DFA.numstates dfa]
        <*> S.toList (DFA.alphabet dfa)

addTransition :: Ord c => GNFA c -> (Integer, c, Integer) -> GNFA c
addTransition gnfa (a, letter, b) = GNFA $ transitions gnfa //
    [((a,b), (transitions gnfa ! (a,b)) `Regex.union` Regex.char letter)]

-- Get the transition start -> end if the state elim is removed
getNewTransition :: Ord c => GNFA c -> Integer -> (Integer, Integer) -> Regex c
getNewTransition gnfa elim (start, end) =
    (r1 `Regex.conc` (Regex.star r2) `Regex.conc` r3) `Regex.union` r4
    where
        r1 = transitions gnfa ! (start, elim)
        r2 = transitions gnfa ! (elim, elim)
        r3 = transitions gnfa ! (elim, end)
        r4 = transitions gnfa ! (start, end)

-- Remove a state from a gnfa
removeState :: Ord c => GNFA c -> Integer -> GNFA c
removeState gnfa elim
    | elim == 1 = error "Cannot remove start state"
    | elim == 2 = error "Cannot remove accept state"
    | otherwise = GNFA $ A.array ((1,1),(n-1,n-1)) newtransitions
    where
        (_,(n,_)) = A.bounds $ transitions gnfa
        indices = (,) <$> [1..n-1] <*> [1..n-1]
        newtransitions = (\x -> (x, getNewTransition gnfa elim $ toOldStates x)) <$> indices
        toOldState x = if x < elim then x else x + 1
        toOldStates (x,y) = (toOldState x, toOldState y)

-- Extract the finished regex from a gnfa
extractRegex :: GNFA c -> Regex c
extractRegex gnfa
    | A.bounds (transitions gnfa) == ((1,1),(2,2))  = transitions gnfa ! (1,2)
    | otherwise                                     = error "GNFA has too many states to extract regex"

-- Convert a dfa to regex by removing states in the order given
dfaToRegexByOrder :: Ord c => [Integer] -> DFA c -> Regex c
dfaToRegexByOrder order dfa = extractRegex $ foldl removeState (fromDFA dfa) fixedOrder where
    fixedOrder = zipWith (\x i -> ((x - 1) `mod` i) + 3) order [n,n-1..1]
    n = DFA.numstates dfa

--
