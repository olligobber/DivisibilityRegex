module UnionFind (
    UnionFind,
    UnionFindS,
    new,
    find,
    union,
    toArray
) where

import Data.Array (Array, (!), Ix, (//))
import qualified Data.Array as A
import Control.Monad.State (State)
import qualified Control.Monad.State as S

newtype UnionFind i = UnionFind { member :: Array i (UnionElement i) }

type UnionFindS i = State (UnionFind i)

data UnionElement i = ChildOf i | RootRank Integer

getRank :: UnionElement i -> Integer
getRank (RootRank x) = x
getRank _ = error "Element is not a root"

-- Make a unionfind where everything is seperate given the bounds
new :: Ix i => (i, i) -> UnionFind i
new bounds = UnionFind $ A.listArray bounds $ repeat $ RootRank 0

-- Extract a union element from a unionfind
extract :: Ix i => i -> UnionFindS i (UnionElement i)
extract mem = S.gets $ \union -> member union ! mem

-- Set a union element's parent
setparent :: Ix i => i -> i -> UnionFindS i ()
setparent child parent = S.modify $ \union -> UnionFind $ member union // [(child, ChildOf parent)]

-- Set a union element as a root with a given rank
setrank :: Ix i => i -> Integer -> UnionFindS i ()
setrank root rank = S.modify $ \union -> UnionFind $ member union // [(root, RootRank rank)]

-- Find the root of an element's union
find :: Ix i => i -> UnionFindS i i
find mem = do
    memele <- extract mem
    case memele of
        RootRank _ -> return mem
        ChildOf par -> do
            root <- find par
            setparent mem root
            return root

-- Join two sets
union :: Ix i => i -> i -> UnionFindS i ()
union mem1 mem2 = do
    root1 <- find mem1
    root2 <- find mem2
    if root1 == root2 then return () else do
        rank1 <- getRank <$> extract root1
        rank2 <- getRank <$> extract root2
        case compare rank1 rank2 of
            GT -> setparent root2 root1
            LT -> setparent root1 root2
            EQ -> do
                setparent root1 root2
                setrank root2 $ rank2 + 1

-- Extract an array mapping each element to root of its union
toArray :: Ix i => UnionFindS i (Array i i)
toArray = do
    bounds <- S.gets $ A.bounds . member
    indices <- S.gets $ A.indices . member
    roots <- mapM find indices
    return $ A.listArray bounds roots
