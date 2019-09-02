module Regulars (
    DFA,
    alphabet,
    numstates,
    move,
    accepts,
    parse,
    dfaRecurse,
    dfaFromMap,
    minimise
) where

import Data.Map.Lazy (Map, (!?))
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Array (Array, (!))
import qualified Data.Array as A
import qualified UnionFind as U
import qualified Control.Monad.State as State
import Data.Maybe (maybe, fromMaybe)
import Control.Monad (guard)
import Data.Function (on)

-- States in a DFA are represented by integers 1..n, where 1 is the start state
newtype DFA c = DFA {
    states :: Array Integer (DFAState c)
    }

data DFAState c = DFAState {
    accepting :: Bool,
    transition :: Map c Integer
    }

-- Gets the letters available for a DFA to parse
alphabet :: DFA c -> Set c
alphabet = M.keysSet . transition . (! 1) . states

-- Gets the number of states in a DFA
numstates :: DFA c -> Integer
numstates = snd . A.bounds . states

-- Perform a single transition along a DFA
move :: Ord c => DFA c -> Integer -> c -> Integer
move dfa state letter
    | A.bounds (states dfa) `A.inRange` state =
        fromMaybe (error "DFA transition failed: letter not in alphabet") $
            transition (states dfa ! state) !? letter
    | otherwise = error "DFA transition failed: state not in DFA"

-- Check if a dfa state accepts
accepts :: DFA c -> Integer -> Bool
accepts dfa = accepting . (states dfa !)

-- Parse a string
parse :: Ord c => DFA c -> [c] -> Bool
parse dfa = accepts dfa . foldl (move dfa) 1

-- Partially constructed DFA
data PartialDFA s c = PartialDFA {
    statemapping :: Map s Integer, -- mapping from state names to enumeration
    completestates :: [(Integer, DFAState c)], -- fully constructed DFA states
    incompletestates :: [(Integer, s)], -- DFA states that need to be constructed
    num :: Integer -- number of DFA states both constructed and not yet
    }

-- Partiall constructed DFA state
data PartialState s c = PartialState {
    statemapping' :: Map s Integer, -- mapping from state names to enumeration
    newstates :: [(Integer, s)], -- states that this state needs to be constructed in the DFA
    num' :: Integer, -- number of states in the DFA plus those this state needs
    transitions :: Map c Integer -- this state's transitions
}

newpartialstate :: Map s Integer -> Integer -> PartialState s c
newpartialstate mapping thisnum = PartialState mapping [] thisnum M.empty

procpartialstate :: (Ord s, Ord c) => s -> (s -> c -> s) -> PartialState s c -> c -> PartialState s c
procpartialstate statename trans partstate letter =
    case statemapping' partstate !? nextstatename of
        Just nextstate -> modifystate
            id
            id
            id
            (M.insert letter nextstate)
        Nothing -> modifystate
            (M.insert nextstatename newstatenum)
            ((newstatenum, nextstatename):)
            succ
            (M.insert letter newstatenum)
    where
        newstatenum = num' partstate + 1
        nextstatename = trans statename letter
        modifystate a b c d = PartialState
            (a $ statemapping' partstate)
            (b $ newstates partstate)
            (c $ num' partstate)
            (d $ transitions partstate)

newpartialdfa :: s -> PartialDFA s c
newpartialdfa start = PartialDFA (M.singleton start 1) [] [(1,start)] 1

procpartialdfa :: (Ord s, Ord c) => (s -> Bool) -> [c] -> (s -> c -> s) -> PartialDFA s c -> Either (DFA c) (PartialDFA s c)
procpartialdfa accept alph trans partdfa = case incompletestates partdfa of
    [] -> Left $ DFA $ A.array (1, num partdfa) (completestates partdfa)
    (statenum,statename):remainincomplete ->
        let
            procstate = foldl
                (procpartialstate statename trans)
                (newpartialstate (statemapping partdfa) (num partdfa)) alph
            newstate = DFAState (accept statename) (transitions procstate)
        in
            Right $ PartialDFA
                (statemapping' procstate)
                ((statenum, newstate):completestates partdfa)
                (newstates procstate ++ remainincomplete)
                (num' procstate)

finishpartialdfa :: (Ord s, Ord c) => (s -> Bool) -> [c] -> (s -> c -> s) -> PartialDFA s c -> DFA c
finishpartialdfa accept alph trans partdfa = case procpartialdfa accept alph trans partdfa of
    Left complete -> complete
    Right incomplete -> finishpartialdfa accept alph trans incomplete

{- make a dfa from:
    a start state,
    a function to determine if a state accepts,
    an alphabet,
    a transition function
-}
dfaRecurse :: (Ord s, Ord c) => s -> (s -> Bool) -> [c] -> (s -> c -> s) -> DFA c
dfaRecurse start accept alph trans = finishpartialdfa accept alph trans $ newpartialdfa start

{- make a dfa from:
    a start state,
    a set of accept states,
    an alphabet,
    a map containing (state, letter) -> state transitions
-}
dfaFromMap :: (Ord s, Ord c) => s -> Set s -> [c] -> Map (s,c) s -> DFA c
dfaFromMap start accept alph trans = dfaRecurse
    (Just start)
    (maybe False (`S.member` accept))
    alph
    (maybe (const Nothing) $ curry (trans !?))

type EqTable = Set (Integer, Integer)

-- Make the set of all pairs of states where both accept or both don't accept
startEqTable :: DFA c -> EqTable
startEqTable dfa = S.fromList $ do
    (firststatenum, firststate) <- A.assocs $ states dfa
    (secondstatenum, secondstate) <- A.assocs $ states dfa
    guard $ ((==) `on` accepting) firststate secondstate
    return (firststatenum, secondstatenum)

-- Removes a pair from the table if taking any transition leads to a pair not in the table
updatePair :: Ord c => DFA c -> EqTable -> (Integer, Integer) -> EqTable
updatePair dfa table pair@(firststatenum, secondstatenum)
    | equiv     = table
    | otherwise = S.delete pair table
    where
        bimap = (M.intersectionWith (,) `on` transition . (states dfa !)) firststatenum secondstatenum
        equiv = all (`S.member` table) bimap

-- Update every pair in the table once
updateAllPairs :: Ord c => DFA c -> EqTable -> EqTable
updateAllPairs dfa table = foldl (updatePair dfa) table table

-- Update the table until it is fixed
solveTable :: Ord c => DFA c -> EqTable -> EqTable
solveTable dfa table
    | nextTable == table    = table
    | otherwise             = solveTable dfa nextTable
    where
        nextTable = updateAllPairs dfa table

-- Convert a table to an array mapping equivalent states to the same number
convertTable :: DFA c -> EqTable -> Array Integer Integer
convertTable dfa table = State.evalState (allunions >> U.toArray) startuf where
    allunions = mapM_ (uncurry U.union) table
    startuf = U.new $ A.bounds $ states dfa

-- Map each state in a DFA to some equivalent state so that all equivalent states are mapped to the same
eqMapping :: Ord c => DFA c -> Integer -> Integer
eqMapping = (!) . (startEqTable >>= flip solveTable >>= flip convertTable)

-- Minimise the number of states a DFA has
minimise :: Ord c => DFA c -> DFA c
minimise dfa = dfaRecurse
    (newstate 1)
    (accepts dfa)
    (S.toList $ alphabet dfa)
    (move dfa . newstate)
    where
        newstate = eqMapping dfa
