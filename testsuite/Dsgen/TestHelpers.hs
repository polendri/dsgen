module Dsgen.TestHelpers where

import Control.Monad(liftM)
import Test.QuickCheck


elemAny :: Eq a => [a] -> [a] -> Bool
elemAny [] _ = False
elemAny (e:es) l = if elem e l then True else elemAny es l

notElemAny :: Eq a => [a] -> [a] -> Bool
notElemAny es l = not $ elemAny es l

allElems :: Eq a => [a] -> [a] -> Bool
allElems [] _ = True
allElems (e:es) l = elem e l && allElems es l

-- | Picks a random subset from the provided list
subset :: [a] -> Gen [a]
subset [] = return []
subset (x:xs) = do
    pickX <- arbitrary :: Gen Bool
    if pickX then liftM (x:) rest else rest
  where rest = subset xs

