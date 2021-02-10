{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types (VectorSpace(..),
             Algebra(..)) where

class (Num b) => VectorSpace a b | b -> a where
   (-*-) :: b -> a -> a
   (-+-) :: a -> a -> a


class (Num b, VectorSpace a b) => Algebra a b where
  (-**-) :: a -> a -> a
