module Trie
  ( Trie (..)
  , get
  , put
  , remove
  , foldd
  ) where

import qualified Data.Foldable as F
import qualified Data.Map      as M

data Trie = Trie
              { term  :: Bool
              , value :: String
              , arcs  :: M.Map Char (Trie)
              } deriving (Show, Eq)

empty :: Trie
empty = Trie False "" M.empty

leaf :: String -> Trie
leaf s = Trie True s M.empty

get :: String -> Trie -> Maybe String
get "" (Trie True v _) = Just v
get "" _ = Nothing
get (x:xs) t@(Trie term value arcs) = case M.lookup x (arcs) of
                                        Nothing -> Nothing
                                        Just v  -> get xs v

put :: String -> String -> Trie -> (Maybe String, Trie)
put "" v t@(Trie term cur arcs) = (prev, updated) where
  prev = case term of
    True  -> Just cur
    False -> Nothing
  updated = t { term = True, value = v }
put (k:ks) v t@(Trie term cur arcs) = (prev, updated) where
  (prev, t') = put ks v childTree
  updated = t { arcs = M.insert k t' arcs }
  childTree = case M.lookup k arcs of
    Nothing    -> empty
    Just child -> child

remove :: String -> Trie -> (Maybe String, Trie)
remove "" t@(Trie term cur _) = case term of
  True  -> (Just cur, t { term = False })
  False -> (Nothing, t)
remove (k:ks) t@(Trie _ cur arcs) = (prev, updated) where
  (prev, t') = remove ks child
  updated = t { arcs = M.delete k arcs }
  child = case M.lookup k arcs of
    Nothing    -> empty
    Just child -> child

foldd :: ((String, String) -> a -> a) -> a -> Trie -> a
foldd = undefined
-- foldd = foldd' ""

--     keyAcc      f                            acc    t
-- foldd' String -> ((String, String) -> a -> a) -> a -> Trie -> a
-- foldd' keyAcc f acc (Trie term value M.empty) = case term of
--   True -> f ((keyAcc, value)) acc
--   False -> acc
-- foldd' keyAcc f acc t@(Trie term value arcs) = foldr f acc (map    )

-- instance F.Foldable Trie where
--  foldMap _ (Trie False _ _) = mempty
--  foldMap f (Trie True cur arcs) = foldMap f

-- foldd :: ((String, String) -> a -> a) -> a -> Trie -> a
-- foldd f acc t = foldd' f acc "" t
--
-- foldd' :: ((String, String) -> a -> a) -> a -> a -> Trie -> a
-- foldd' f acc keyAcc t@(Trie term cur arcs) = case term of
--   True -> f ((keyAcc, cur) acc)
