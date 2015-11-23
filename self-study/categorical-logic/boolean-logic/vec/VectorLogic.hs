import Data.Category
import Data.Category.Functor
import ClosedMonoidalCategory


data LinearMap u v = LinearMap { unLinMap  :: u -> v
                               }

instance Category LinearMap where
-- src = LinearMap id

