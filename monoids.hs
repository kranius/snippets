import Data.Monoid

newtype Or a = Or a
  deriving (Eq, Ord)

getOr (Or a) = a

instance Bool a => Monoid (Or a) where
  mempty  = Or False
  mappend = (||)
