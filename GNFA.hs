module GNFA (
    dfaToRegexByOrder,
    dfaToRegexMinimal,
    dfaToRegexRandom,
    dfaToRegexRandomAttempts
) where

import DFA (DFA)
import qualified DFA
import Regex (Regex)
import qualified Regex
import Data.Array (Array, (!), (//))
import qualified Data.Array as A
import qualified Data.Set as S
import System.Random (RandomGen, randomR)
import Data.Semigroup (stimesMonoid, Endo(..), appEndo)

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
    (r1 `Regex.conc` Regex.star r2 `Regex.conc` r3) `Regex.union` r4
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

-- Get all possible orders to remove states
allOrders :: Integer -> [[Integer]]
allOrders 0 = [[]]
allOrders n
    | n > 0     = (:) <$> [1..n] <*> allOrders (n-1)
    | otherwise = error "Cannot remove a negative number of states"

-- Convert a dfa to regex by removing states in all possible orders and keeping the shortest regex
dfaToRegexMinimal :: Ord c => DFA c -> Regex c
dfaToRegexMinimal dfa = foldl update (dfaToRegexByOrder (repeat 1) dfa) (allOrders n) where
    n = DFA.numstates dfa
    update regex order = Regex.smallest regex (dfaToRegexByOrder order dfa)

-- Get a random order to remove states
randomOrder :: RandomGen g => g -> Integer -> (g, [Integer])
randomOrder gen 0 = (gen, [])
randomOrder gen n
    | n < 0     = error "Cannot remove a negative number of states"
    | otherwise = let (x, gen') = randomR (1,n) gen in
        (x:) <$> randomOrder gen' (n-1)

-- Convert a dfa to regex by removing states in a random order
dfaToRegexRandom :: (Ord c, RandomGen g) => g -> DFA c -> (g, Regex c)
dfaToRegexRandom gen dfa = flip dfaToRegexByOrder dfa <$> randomOrder gen (DFA.numstates dfa)

-- Given a regex, try to make a shorter one
dfaAttempt :: (Ord c, RandomGen g) => DFA c -> Endo (g, Regex c)
dfaAttempt dfa = Endo $ \(gen, regex) ->
    Regex.smallest regex <$> dfaToRegexRandom gen dfa

-- Convert a dfa to regex by trying some number of random attempts and picking the smallest
dfaToRegexRandomAttempts :: (Ord c, RandomGen g) => g -> Integer -> DFA c -> (g, Regex c)
dfaToRegexRandomAttempts gen n dfa
    | n < 1     = error "Cannot make less than one attempt"
    | otherwise = appEndo (stimesMonoid (n-1) (dfaAttempt dfa)) $ dfaToRegexRandom gen dfa
