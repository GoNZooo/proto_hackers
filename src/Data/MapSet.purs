module Data.MapSet
  ( MapSet
  , empty
  , toList
  , insert
  ) where

import Erl.Data.List (List)

foreign import data MapSet :: Type -> Type

foreign import empty :: forall a. MapSet a
foreign import toList :: forall a. MapSet a -> List a
foreign import insert :: forall a. a -> MapSet a -> MapSet a
