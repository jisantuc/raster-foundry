module Data.RasterFoundry.Classes.Filter (
  Filter,
  orFilter,
  andFilter
  ) where

class Filterable a where
  toFilter :: a -> Filter

newtype Filter = Filter String

instance Filterable Filter where
  toFilter = id

newtype AndFilter = AndFilter Filter

instance Filterable AndFilter where
  toFilter (AndFilter f) = f

instance Semigroup AndFilter where
  (<>) this that =
    case (toFilter this, toFilter that) of
      (Filter "", Filter "") -> AndFilter (Filter "")
      (s, Filter "")  -> AndFilter s
      (Filter "", s)  -> AndFilter s
      (s, t)   -> AndFilter $ s `and_` t

instance Monoid AndFilter where
  mempty = AndFilter (Filter "")

newtype OrFilter = OrFilter Filter

combine :: String -> Filter -> Filter -> Filter
combine sep (Filter t) (Filter u) = Filter (t ++ sep ++ u)

and_ :: Filter -> Filter -> Filter
and_ = combine " AND "

or_ :: Filter -> Filter -> Filter
or_ = combine " OR "

andFilter :: String -> AndFilter
andFilter = AndFilter . Filter

orFilter :: String -> OrFilter
orFilter = OrFilter . Filter
