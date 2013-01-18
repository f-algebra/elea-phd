module Elea.Notes.Show
(
  HasNote (..), Note, 
  tag, tagVar, tagAlts,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term (..) , InnerTerm (..), Alt (..) )

import qualified Elea.Term as Term
import qualified Data.Map as Map
import qualified Data.Label.Maybe as Maybe
import qualified Control.Monad.State as State

data Note
  = Note  { _showAs :: Maybe String
          , _varName :: Maybe String 
          , _altVars :: Maybe [[String]] }
              
mkLabels [''Note]

tag :: HasNote a => String -> Term a -> Term a
tag name = modify Term.notes (set (showAs . note) (Just name)) 
  
tagVar :: HasNote a => String -> Term a -> Term a
tagVar name = modify Term.notes (set (varName . note) (Just name))

tagAlts :: HasNote a => [[String]] -> Term a -> Term a
tagAlts alts = modify Term.notes (set (altVars . note) (Just alts))

class Term.Notes a => HasNote a where
  note :: a :-> Note 

instance (Term.Notes b, HasNote a) => HasNote (a, b) where
  note = note . fst

instance Monoid Note where
  mempty = Note mempty mempty mempty

  Note tag1 var1 as1 `mappend` Note tag2 var2 as2 = 
    Note (tag1 `mplus` tag2) 
         (var1 `mplus` var2) 
         (as1 `mplus` as2)

instance Term.Notes Note where

instance HasNote Note where
  note = id
              
data ShowEnv = 
  ShowEnv { _indent :: Int
          , _vars :: [String] }

emptyEnv = ShowEnv 0 empty
          
mkLabels [''ShowEnv]
    
instance HasNote a => Show (Term a) where
  show term = evalState (showR term) emptyEnv
    where
    showVar :: Maybe String -> State ShowEnv String
    showVar mby_name = do
      var_count <- State.gets (length . get vars) 
      let backup_name = "_" ++ show var_count
          name | Just nme <- mby_name = nme
               | otherwise = backup_name
               
      is_used <- State.gets (elem name . get vars)
      let name' | is_used = backup_name
                | otherwise = name
      State.modify (modify vars (name':))
      return name'

    popVars :: Int -> State ShowEnv ()
    popVars n = State.modify (modify vars (drop n))
      
    showR :: Term a -> State ShowEnv String
    showR term@(Term n iterm)
      | Just s <- get (showAs . note) n = return s
      --  , not (Term.isFix term) = s 
      | otherwise = showI iterm
      where
      showI :: InnerTerm a -> State ShowEnv String
      showI Absurd = return "_|_"
      showI (Inj n) = return $ "in" ++ show n
      showI (Var idx) = do
        State.gets ((!! (fromEnum idx)) . get vars)
      showI (App t1 t2) = do
        t1_s <- showR t1
        t2_s <- showR t2
        let t2_s' | ' ' `elem` t2_s = "(" ++ t2_s ++ ")"
                  | otherwise = t2_s
        return 
          $ t1_s ++ " " ++ t2_s'
      showI (Lam _) = do
        vars <- mapM (showVar . get (varName . note)) ns
        let vars_s = intercalate " " vars
        rhs_s <- showR rhs
        popVars (length vars)
        return
          $ "(lam " ++ vars_s ++ " -> " ++ rhs_s ++ ")"
        where
        (ns, rhs) = Term.flattenLam term
      showI (Fix fix_rhs) = do
        vars <- mapM (showVar . get (varName . note)) (n:ns)
        let vars_s = intercalate " " vars
        rhs_s <- showR rhs
        popVars (length vars)
        return
          $ "(fix " ++ vars_s ++ " -> " ++ rhs_s ++ ")"
        where
        (ns, rhs) = Term.flattenLam fix_rhs
      showI (Case lhs alts) = do
        lhs_s <- showR lhs
        alts_s <- liftM (intercalate " ")
          $ zipWithM showAlt alt_vs alts
        return 
          $ "(case " ++ lhs_s ++ " " ++ alts_s ++ ")" 
        where
        alt_vs :: [Maybe [String]]
        alt_vs = sequence (get (altVars . note) n)
        
        showAlt :: Maybe [String] -> Alt a -> State ShowEnv String
        showAlt mby_vs (Alt n rhs) = do
          vars_s <- mapM showVar vars
          let cons_s = fromMaybe "??" cons  
          rhs_s <- showR rhs
          popVars (length vars)
          return
            $ "(" ++ intercalate " " (cons_s:vars_s)
              ++ " -> " ++ rhs_s ++ ")"
          where
          (cons:vars) = sequence mby_vs
            
instance HasNote a => Show (InnerTerm a) where
  show = show . Term mempty
    

