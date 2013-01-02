module Elea.Parser
(
  toplevel, term
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( InnerTerm (..), Term (..), Alt (..), Note (..) )
import Elea.Lisp ( Lisp (..), InnerLisp (..) )

import qualified Elea.Term as Term
import qualified Elea.Lisp as Lisp
import qualified Elea.Monad.Error as Error
import qualified Elea.Monad.Definitions as Defs

toplevel :: forall a m . (Defs.Monad a m, Error.Monad m) => String -> m ()
toplevel txt = do
  Lisp _ (List defs) <- Lisp.parse txt
  mapM_ parseDef defs
  where
  parseDef :: Lisp -> m ()
  parseDef (Lisp _ (List [def, name, val]))
    | "def" <- Lisp.fromAtom def
    , name <- Lisp.fromAtom name = do
        val_t :: Term a <- runReaderT (parseTerm val) mempty
        Defs.add name val_t
        
  parseDef lisp = 
    Error.throw $ "Not a valid definition:\n" ++ show lisp

term :: (Defs.Monad a m, Error.Monad m) => String -> m (Term a)
term txt = do
  lisp <- Lisp.parse txt
  runReaderT (parseTerm lisp) mempty
  
pushVar :: MonadReader [String] m => String -> m a -> m a
pushVar name = local ([name] ++)

pushVars :: MonadReader [String] m => [String] -> m a -> m a
pushVars = concatEndos . map pushVar

parseTerm :: forall m a . (MonadReader [String] m, 
    Defs.Monad a m, Error.Monad m) => 
  Lisp -> m (Term a) 
parseTerm (Lisp raw_notes inner) = parseInner inner
  where
  returnTerm term = do
    notes <- noteFromLisp raw_notes 
    return (Term notes term)
  
  parseInner :: InnerLisp -> m (Term a)
  parseInner (Atom "_|_") = 
    returnTerm Absurd
  parseInner (Atom ('i':'n':n)) = 
    returnTerm (Inj (read n))
  
  parseInner (Atom n) = do
    binds <- ask
    case elemIndex n binds of
      Just i -> returnTerm (Var (toEnum i))
      Nothing -> do
        mby_t <- liftM (map Term.inner) (Defs.lookup n)
        maybe (Error.throw doesnt_exist) returnTerm mby_t
    where
    doesnt_exist = "Identifier " ++ show n ++ " not found."
  
  parseInner (List ((Lisp.fromAtom -> "fix") : rest)) = do
    notes <- noteFromLisp raw_notes
    liftM (Term notes . Fix)
      $ pushVar fix_var
      $ liftM (Term.unflattenLam (length lam_vars))
      $ pushVars lam_vars (parseTerm body)
    where
    body = last rest
    (fix_var:lam_vars) = map Lisp.fromAtom (init rest)
    
  parseInner (List ((Lisp.fromAtom -> "lam") : rest)) = 
    liftM (Term.unflattenLam (length vars))
    $ pushVars vars
    $ parseTerm body
    where
    body = last rest
    vars = map Lisp.fromAtom (init rest)
    
  parseInner (List ((Lisp.fromAtom -> "case") : cse_of : alts)) = do
    alts_t <- zipWithM parseAlt [0..] alts
    cse_of_t <- parseTerm cse_of
    returnTerm (Case cse_of_t alts_t)
    where 
    parseAlt :: Int -> Lisp -> m (Alt a)
    parseAlt n (Lisp _ (List (cons : rest))) = do
      cons_t <- parseTerm cons :: m (Term a)
      
      Error.unless (all Lisp.isAtom vars)
        $ "Term in case-alt vars"
      Error.unless (cons_t == Term mempty (Inj n))
        $ "Invalid constructor"
      
      liftM (Alt n)
        $ pushVars var_names 
        $ parseTerm body
      where
      vars = init rest
      body = last rest
      var_names = map Lisp.fromAtom vars
    
  parseInner (List xs) = 
    liftM Term.unflattenApp (mapM parseTerm xs)

