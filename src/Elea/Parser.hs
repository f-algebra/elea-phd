module Elea.Parser
(
  term
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term (..), Alt (..) )
import Elea.ELisp ( ELisp (..) )

import qualified Elea.Term as Term
import qualified Elea.ELisp as ELisp
import qualified Elea.Error as Error
import qualified Elea.Definitions as Defs

term :: forall m . (Defs.Monad m, Error.Monad m) => String -> m Term
term txt = do
  elisp <- ELisp.parse txt
  runReaderT (el elisp) mempty
  where
  pushVar :: String -> ReaderT [String] m a -> ReaderT [String] m a
  pushVar name = local ([name] ++)
  
  pushVars = concatEndos . map pushVar
  
  el :: ELisp -> ReaderT [String] m Term 
  el (Atom "_|_") = return Absurd
  el (Atom ('i':'n':n)) = return (Inj (read n))
  
  el (Atom n) = do
    binds <- ask
    case elemIndex n binds of
      Just i -> return (Var (toEnum i))
      Nothing -> do
        mby_t <- Defs.lookup n
        maybe (Error.throw doesnt_exist) return mby_t
    where
    doesnt_exist = "Identifier " ++ show n ++ " not found."

  el (List (Atom "fix":rest)) = 
    liftM Fix
    $ pushVar fix_var
    $ liftM (Term.unflattenLam (length lam_vars))
    $ pushVars lam_vars (el body)
    where
    body = last rest
    (fix_var:lam_vars) = map ELisp.fromAtom (init rest)
    
  el (List (Atom "\\":rest)) = 
    liftM (Term.unflattenLam (length vars))
    $ pushVars vars
    $ el body
    where
    body = last rest
    vars = map ELisp.fromAtom (init rest)
    
  el (List (Atom "case" : cse_of : alts)) = do
    alts_t <- liftM (zipWith Alt [0..]) (mapM el alts)
    cse_of_t <- el cse_of
    return (Case cse_of_t alts_t)
    
  el (List xs) = 
    liftM Term.unflattenApp (mapM el xs)

