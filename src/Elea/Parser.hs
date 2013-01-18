-- | Turn code that has been run through Elea.Lisp.parse into 'Term's
module Elea.Parser
(
  toplevel, term
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( InnerTerm (..), Term (..), Alt (..), Notes (..) )
import Elea.Lisp ( Lisp (..), InnerLisp (..) )

import qualified Elea.Term as Term
import qualified Elea.Lisp as Lisp
import qualified Elea.Monad.Error as Error
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Notes.Show as Show
import qualified Data.Map as Map
import qualified Data.Label.Maybe as Maybe

toplevel :: forall a m . 
    (Show.HasNote a, Defs.Monad a m, Error.Monad m) => 
  String -> m ()
toplevel txt = do
  Lisp _ (List defs) <- Lisp.parse txt
  mapM_ parseDef defs
  where
  parseDef :: Lisp -> m ()
  parseDef (Lisp _ (List [def, name, val]))
    | Just "def" <- Lisp.fromAtom def
    , Just name <- Lisp.fromAtom name = do
        val_t :: Term a <- runReaderT (parseTerm val) mempty
        Defs.add name val_t
        
  parseDef lisp = 
    Error.throw $ "Not a valid definition:\n" ++ show lisp

term :: (Show.HasNote a, Defs.Monad a m, Error.Monad m) => 
  String -> m (Term a)
term txt = do
  lisp <- Lisp.parse txt
  runReaderT (parseTerm lisp) mempty
  
pushVar :: MonadReader [String] m => String -> m a -> m a
pushVar name = local ([name] ++)

pushVars :: MonadReader [String] m => [String] -> m a -> m a
pushVars = concatEndos . map pushVar

constructLams :: forall m a . (Error.Monad m, Show.HasNote a) => 
  [Lisp] -> Term a -> m (Term a)
constructLams = concatEndosM . map constructLam . reverse
  where
  constructLam :: Lisp -> Term a -> m (Term a)
  constructLam lisp rhs =
    liftM (Show.tagVar var_name)
    $ buildTerm (get Lisp.notes lisp) (Lam rhs)
    where
    Just var_name = Lisp.fromAtom lisp
  
buildTerm :: (Error.Monad m, Show.HasNote a) => 
  Map String Lisp -> InnerTerm a -> m (Term a)
buildTerm lisp inner = do
  notes <- notesFromLisp inner lisp
  return (Term notes inner)
  
parseTerm :: forall m a . (Show.HasNote a,
    MonadReader [String] m, Defs.Monad a m, Error.Monad m) => 
  Lisp -> m (Term a) 
parseTerm (Lisp notes inner) = parseInner inner
  where
  parseInner :: InnerLisp -> m (Term a)
  parseInner (Atom "_|_") = 
    buildTerm notes Absurd
  parseInner (Atom ('i':'n':n)) = 
    buildTerm notes (Inj (read n))
  
  parseInner (Atom n) = do
    binds <- ask
    case elemIndex n binds of
      Just i ->
        liftM (Show.tagVar n)
        $ buildTerm notes (Var (toEnum i))
      Nothing -> do
        mby_t <- Defs.lookup n
        case mby_t of
          Nothing -> Error.throw
            $ "Identifier " ++ show n ++ " not found."
          Just term -> do
            more_notes <- notesFromLisp (get Term.inner term) notes
            let term' = modify Term.notes (more_notes ++) term
            return (Show.tag n term')
  
  parseInner (List ((Lisp.fromAtom -> Just "fix") : rest)) = do
    rhs <- pushVars var_names (parseTerm body)
    lam_rhs <- constructLams lam_vars rhs
    liftM (Show.tagVar (head var_names))
      $ buildTerm notes (Fix lam_rhs)
    where
    Just var_names = sequence (map Lisp.fromAtom vars)
    body = last rest
    vars@(fix_var:lam_vars) = init rest
    
  parseInner (List ((Lisp.fromAtom -> Just "lam") : rest)) = do
    rhs <- pushVars var_names (parseTerm body)
    lam_rhs <- constructLams vars rhs
    more_notes <- notesFromLisp (get Term.inner lam_rhs) notes
    return (modify Term.notes (++ more_notes) lam_rhs)
    where
    Just var_names = sequence (map Lisp.fromAtom vars) 
    body = last rest
    vars = init rest
    
  parseInner (List ((Lisp.fromAtom -> Just "case") : cse_of : alts)) = do
    (alt_ts, var_names) <- liftM unzip
      $ zipWithM parseAlt [0..] alts
    cse_of_t <- parseTerm cse_of
    liftM (Show.tagAlts var_names)
      $ buildTerm notes (Case cse_of_t alt_ts)
    where 
    parseAlt :: Int -> Lisp -> m (Alt a, [String])
    parseAlt n (Lisp _ (List rest)) = do
      cons_t <- parseTerm cons :: m (Term a)
      
      Error.unless (all Lisp.isAtom vars)
        $ "Term in case-alt vars"
      Error.unless (get Term.inner cons_t == Inj n)
        $ "Invalid constructor"
      
      alt_t <- liftM (Alt n)
        $ pushVars var_names 
        $ parseTerm body
      return (alt_t, [show cons_t] ++ var_names)
      where
      (cons:vars) = init rest
      body = last rest
      Just var_names = sequence (map Lisp.fromAtom vars)
    
  parseInner (List xs) = 
    liftM Term.unflattenApp (mapM parseTerm xs)

