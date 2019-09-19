module GNFA (
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
