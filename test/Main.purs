module Test.Main where

import Batteries

main :: forall e. Eff (assert :: ASSERT | e) Unit
main = do
  assert true
