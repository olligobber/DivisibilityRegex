{-# LANGUAGE RankNTypes #-}

module UnionFind (
    Getter,
    Setter,
    UnionFind,
    UnionFindS,
    new,
    find,
    union,
    flatten
) where

import Control.Monad.State (State)
import qualified Control.Monad.State as S

-- Gets a value from a map-like type
type Getter m i = forall v. m v -> i -> v

-- Sets a value in a map-like type
type Setter m i = forall v. m v -> i -> v -> m v

-- UnionFind based on an arbitrary map-like type
data UnionFind m i = UnionFind {
    getter :: Getter m i,
    setter :: Setter m i,
    members :: m (UnionElement i),
    identity :: m i
}

data UnionElement i = ChildOf i | RootRank Integer

-- Specialised setters and getters for UnionFind
infix 9 !
infix 9 //

(!) :: UnionFind m i -> i -> UnionElement i
uf ! index = getter uf (members uf) index

(//) :: UnionFind m i -> (i, UnionElement i) -> UnionFind m i
uf // (index, value) = UnionFind
    (getter uf)
    (setter uf)
    (setter uf (members uf) index value)
    (identity uf)

getRank :: UnionElement i -> Integer
getRank (RootRank x) = x
getRank _ = error "Element is not a root"

-- Make a unionfind where everything is seperate given the map-like type prefilled with indices mapping to themselves
new :: Functor m => Getter m i -> Setter m i -> m i -> UnionFind m i
new get set structure = UnionFind get set (RootRank 0 <$ structure) structure

-- Stateful UnionFind operations
type UnionFindS m i = State (UnionFind m i)

-- Stateful getter for unionfind
extract :: i -> UnionFindS m i (UnionElement i)
extract mem = S.gets (! mem)
--
-- Set a unionfind element's parent
setparent :: Eq i => i -> i -> UnionFindS m i ()
setparent child parent
    | child == parent   = error "Tried to make loop in UnionFind"
    | otherwise         = S.modify (// (child, ChildOf parent))

-- Set a unionfind element as a root with a given rank
setrank :: i -> Integer -> UnionFindS m i ()
setrank root rank = do
    rootVal <- extract root
    case rootVal of
        ChildOf _ -> error "Tried to make non-root of UnionFind into a root"
        RootRank _ -> S.modify (// (root, RootRank rank))

-- Find the representative of an element's set
find :: Eq i => i -> UnionFindS m i i
find mem = do
    memele <- extract mem
    case memele of
        RootRank _ -> return mem
        ChildOf par -> do
            root <- find par
            setparent mem root
            return root

-- Join two sets
union :: Eq i => i -> i -> UnionFindS m i ()
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

-- Extract the underlying map-like type mapping each element to its representative
flatten :: (Eq i, Traversable m) => UnionFindS m i (m i)
flatten = S.gets identity >>= mapM find
