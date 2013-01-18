module Elea.Term
(
  Notes (..), Index,
  Term (..), InnerTerm (..), Alt (..),
  Liftable (..), liftMany,
  inner, notes, index,
  updateNotes, leftmost, 
  altVarCount, altTerm,
  caseOfTerm, caseOfAlts,
  isInj, isFix, isLam,
  flattenApp, unflattenApp, flattenLam,
  liftedTransform, liftedFold,
  Substitutions, substMany,
  substAt, substTop,
)
where

import Prelude ()
import Elea.Prelude hiding ( lift )
import Elea.Lisp ( Lisp )
import qualified Elea.Monad.Error as Error
import qualified Data.Set as Set

-- | De-bruijn indices for term variables
newtype Index = Index Int
  deriving ( Eq, Ord )

-- | The annotated, untyped, De-bruijn indexed lambda calculus,
-- with general recursion ('Fix'), 
-- inductive data types ('Inj' and 'Case'),
-- and absurdity ('Absurd').
-- It is parameterised by the type of its annotations.
data Term a
  = Term  { _notes :: a
          , _inner :: !(InnerTerm a) }
  deriving ( Functor )
          
-- | 'Term's without their annotations.
data InnerTerm a
  = Var { _index :: !Index }
  | App !(Term a) !(Term a)
  | Lam !(Term a)
  | Fix !(Term a)
  | Inj !Int
  | Case { _caseOfTerm :: !(Term a)
         , _caseOfAlts :: ![Alt a] }
  | Absurd 
  deriving ( Eq, Ord, Functor )
  
-- | The branches of a pattern matching 'Case' expression.
data Alt a 
  = Alt { -- | The number of variables this constructor takes.
          _altVarCount :: !Int
        , _altTerm :: !(Term a) }
  deriving ( Eq, Ord, Functor )
  
instance Eq (Term a) where
  (==) = (==) `on` _inner
  
instance Ord (Term a) where
  compare = compare `on` _inner
  
mkLabels [''Term, ''InnerTerm, ''Alt]
  
-- | Generalised 'Term' annotations.
-- Annotations form a 'Monoid', where 'mempty' is unannotated code, and 
-- "a ++ b" takes the union of annotations "a" and "b", favouring "a"
-- if there is a clash.
class Monoid a => Notes a where
  -- | Process the annotations from the 'Lisp' code that produced this term.
  notesFromLisp :: Error.Monad m => InnerTerm a -> Map String Lisp -> m a
  notesFromLisp _ _ = return mempty
  
  -- | A term has been transformed into another. 
  -- Return the new set of notes which should be attached to the new term.
  transformNotes :: Term a -> Term a -> a
  transformNotes old new = 
    get notes new ++ get notes old
  
updateNotes :: Notes a => Term a -> Term a -> Term a 
updateNotes old new = 
  set notes (transformNotes old new) new
  
instance (Notes a, Notes b) => Notes (a, b) where
  notesFromLisp t m = do
    x <- notesFromLisp (map (get fst) t) m
    y <- notesFromLisp (map (get snd) t) m
    return (x, y)
    
  transformNotes t1 t2 =
    ( transformNotes (map (get fst) t1) (map (get fst) t2)
    , transformNotes (map (get snd) t1) (map (get snd) t2) )
  
isInj :: Term a -> Bool
isInj (get inner -> Inj _) = True
isInj _ = False

isFix :: Term a -> Bool
isFix (get inner -> Fix _) = True
isFix _ = False

isLam (get inner -> Lam _) = True
isLam _ = False

flattenApp :: Term a -> [Term a]
flattenApp (get inner -> App t1 t2) = flattenApp t1 ++ [t2]
flattenApp other = [other]

unflattenApp :: Monoid a => [Term a] -> Term a
unflattenApp = foldl1 ((Term mempty .) . App)

flattenLam :: Term a -> ([a], Term a)
flattenLam (Term n (Lam rhs)) = modify fst (n:) (flattenLam rhs)
flattenLam other = ([], other)

-- | Returns the leftmost 'Term' in term application.
-- E.g. "leftmost (App (App a b) c) == a"
leftmost :: Notes a => Term a -> Term a
leftmost = head . flattenApp
 
class Liftable a where
  -- | Creates a new De-Bruijn index 
  -- by shifting all the later ones up by one.
  liftAt :: Index -> a -> a
  
  -- | Increments every free De-Bruijn index by one.
  -- Equivalent to creating a De-Bruijn index at 0 - see 'liftAt'.
  lift :: a -> a
  lift = liftAt (toEnum 0)
  
liftMany :: Liftable a => Int -> a -> a
liftMany 0 = id
liftMany n | n > 0 = lift . liftMany (n-1)
  
instance Liftable Index where
  liftAt at x
    | at <= x = succ x
    | otherwise = x
    
instance (Liftable a, Liftable b) => Liftable (a, b) where
  liftAt i (x, y) = (liftAt i x, liftAt i y)
  
liftedTransform :: (Liftable r, MonadReader r m) => 
  (Term a -> m (Term a)) -> Term a -> m (Term a)
liftedTransform f = trns
  where
  trns (Term ns iterm) = 
    f =<< liftM (Term ns) (trnsI iterm)
  
  trnsI (App t1 t2) =
    return App `ap` trns t1 `ap` trns t2
  trnsI (Lam rhs) = 
    liftM Lam $ local lift $ trns rhs
  trnsI (Fix rhs) = 
    liftM Fix $ local lift $ trns rhs
  trnsI (Case lhs alts) = do
    lhs' <- trns lhs
    alts' <- mapM trnsAlt alts
    return (Case lhs' alts')
    where
    trnsAlt (Alt n rhs) = 
      liftM (Alt n) $ local (liftMany n) $ trns rhs 
  trnsI other = 
    return other
   
liftedFold :: (Liftable r, MonadReader r m, Monoid b) =>
  (Term a -> m b) -> Term a -> m b
liftedFold f = execWriterT . liftedTransform tellAll  
  where
  tellAll t = tell (f t) >> return t
  

-- Here a list of substitutions acts like a "Map Index (Term a)",
-- where the index in the list is the index this term should
-- be substituted for.
type Substitutions a = [Maybe (Term a)]

-- | Apply a series of 'Substitutions' to a term.
substMany :: Notes a => Substitutions a -> Term a -> Term a
substMany = go (toEnum 0)
  where
  go n [] = id
  go n (Nothing : subs) = go (succ n) subs
  go n (Just t : subs) = go n subs . substAt n t
  
-- | Substitute at the outermost De-bruijn index 0
substTop :: Notes a => Term a -> Term a -> Term a
substTop = substAt (toEnum 0)

substAt :: forall a . Notes a => Index -> Term a -> Term a -> Term a
substAt at with term =
  runReader (liftedTransform substL term) (at, with)
  where
  substL :: MonadReader (Index, Term a) m => Term a -> m (Term a)
  substL term@(get inner -> Var var) = do
    (at, with) <- ask
    return $ case at `compare` var of
      -- Substitution occurs
      EQ -> updateNotes term with 
      -- Substitution does not occur
      LT -> set inner (Var (pred var)) term
      GT -> set inner (Var var) term
  substL other = 
    return other

instance Notes a => Liftable (Term a) where
  liftAt at term = runReader (liftedTransform liftL term) at
    where
    liftL :: MonadReader Index m => Term a -> m (Term a)
    liftL term@(get inner -> Var x) = do
      at <- ask
      return $ set inner (Var (liftAt at x)) term
    liftL other = 
      return other

instance Enum Index where
  succ (Index n) = Index (n + 1)
  pred (Index n) | n > 0 = Index (n - 1)
  toEnum n | n >= 0 = Index n
  fromEnum (Index n) = n
  
instance Uniplate (Term a) where
  uniplate (Term note inner) = 
    (str, \str -> Term note (rmk str))
    where
    (str, rmk) = biplate inner
  
instance Biplate (InnerTerm a) (Term a) where
  biplate (Var idx) = 
    (Zero, \Zero -> Var idx)
  biplate (Inj n) =
    (Zero, \Zero -> Inj n)
  biplate Absurd = 
    (Zero, \Zero -> Absurd)
  biplate (App t1 t2) = 
    (Two (One t1) (One t2), \(Two (One t1) (One t2)) -> App t1 t2)
  biplate (Lam rhs) = 
    (One rhs, \(One rhs) -> Lam rhs)
  biplate (Fix rhs) =
    (One rhs, \(One rhs) -> Fix rhs)
  biplate (Case lhs alts) = 
    (Two (One lhs) alt_str, 
      \(Two (One lhs) alt_str) -> Case lhs (mkAlts alt_str))
    where
    alt_str = listStr (map (get altTerm) alts)
    mkAlts alt_str = zipWith mkAlt alts (strList alt_str)
    mkAlt (Alt n _) t = Alt n t
    
instance Show Index where
  show (Index n) = "_" ++ show n

