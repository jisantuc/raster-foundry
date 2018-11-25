module Data.RasterFoundry.Classes.Filter (
  Filterable(..),
  Filter(..),
  AndFilter,
  OrFilter,
  orFilter,
  andFilter,
  toQuery
  ) where

import qualified Database.PostgreSQL.Simple as Postgres

{- The filterable class represents types that can be converted into filters for database queries.
-}
class Filterable a where
  toFilter :: a -> Filter

{- The filter type is a newtype on Postgres.Query that supports combination with AND and OR
in SQL and grouping with parentheses
-}
newtype Filter = Filter Postgres.Query

instance Filterable Filter where
  toFilter = id

newtype AndFilter = AndFilter Filter

instance Filterable AndFilter where
  toFilter (AndFilter f) = f

instance Semigroup AndFilter where
  (<>) this that =
    case (toFilter this, toFilter that) of
      (Filter "", Filter "") -> AndFilter (Filter "")
      (s, Filter "")         -> AndFilter s
      (Filter "", s)         -> AndFilter s
      (s, t)                 -> AndFilter $ s `and_` t

instance Monoid AndFilter where
  mempty = andFilter ""

newtype OrFilter = OrFilter Filter

instance Filterable OrFilter where
  toFilter (OrFilter f) = f

instance Semigroup OrFilter where
  (<>) this that =
    case (toFilter this, toFilter that) of
      (Filter "", Filter "") -> OrFilter (Filter "")
      (s, Filter "")         -> OrFilter s
      (Filter "", s)         -> OrFilter s
      (s, t)                 -> OrFilter $ s `or_` t

instance Monoid OrFilter where
  mempty = orFilter ""

combine :: Postgres.Query -> Filter -> Filter -> Filter
combine sep (Filter t) (Filter u) = Filter (t <> sep <> u)

and_ :: Filter -> Filter -> Filter
and_ = combine " AND "

or_ :: Filter -> Filter -> Filter
or_ = combine " OR "

andFilter :: Postgres.Query -> AndFilter
andFilter = AndFilter . Filter

orFilter :: Postgres.Query -> OrFilter
orFilter = OrFilter . Filter

toQuery :: Filter -> Postgres.Query
toQuery (Filter s) = s

group :: Filter -> Filter
group (Filter s) = Filter . ("(" <>) . (<> ")") $ s
