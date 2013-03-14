module Elea.Fusion 
(
  fusion, fission
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term (..), InnerTerm (..), inner )
import Elea.Context ( Context )

import qualified Elea.Term as Term
import qualified Elea.Context as Context
import qualified Elea.Simplifier as Simplifier
import qualified Elea.Monad.Failure as Fail
{-
potentialConstructorFloat :: forall a m . 
  (Term.Notes a, Fail.Monad m) => 
    Term a -> m (Context a)
potentialConstructorFloat (Term _ (Fix fixed_term)) = do
  ctxs <- runReaderT (Term.liftedFold pcf fixed_term) (toEnum 0)
  if Set.size ctxs == 1
  then return (head (Set.toList ctxs))
  else Fail.here
  where
  pcf :: Term a -> ReaderT Index (Set (Context a)) 
  pcf term@(get inner -> Case _ alts) =
    liftM concat (mapM pcfAlt alts)
    where
    pcfAlt (Alt n rhs) = do 
      
    
  pcf other = return other
  -}
  
fusion :: Monad m => (Term -> m Term) -> Context -> Term -> m (Maybe Term)

fission :: Monad m => (Term -> m Term) -> Context -> Term -> m (Maybe Term)
