-- | The display code for terms and things that contain terms.
module Elea.Show 
(
  module Elea.Show.Class
) 
where

import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Type
import Elea.Context ( Context )
import Elea.Show.Class
import qualified Elea.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Type as Type
import qualified Elea.Context as Context
import qualified Elea.Foldable as Fold
import qualified Data.Map as Map

-- @Term' String@ represents a term where all subterms are replaced by strings. 
-- We use this to represent a term where all subterms have been
-- replaced by their displayed versions. 
-- Similarly @Term' (String, Term)@ is the same as before
-- but the subterms are also included, in case we need information about them.

instance Show Term where
  show = Env.empty . showM
    
bracketIfNeeded :: String -> String
bracketIfNeeded s 
  | ' ' `elem` s = "(" ++ s ++ ")"
  | otherwise = s
 
showWithArgs :: String -> [String] -> String
showWithArgs f xs = f ++ concatMap ((" " ++) . bracketIfNeeded) xs
  
instance Env.Bindings m => ShowM m Term where
  showM = Fold.paraM showM
  
instance Env.Bindings m => ShowM m (Term' (Term, String)) where
  showM (Var' idx xs) = do
    bs <- Env.bindings
    if idx >= length bs
    -- If we don't have a binding for this index 
    -- just display the index itself
    then return (show idx)
    else do
      Bind lbl _ <- Env.boundAt idx
      let lbl' | ' ' `elem` lbl = "\"" ++ lbl ++ "\""
               | otherwise = lbl
               
      -- Count the number of bindings before this one
      -- which have the same label
      let same_lbl_count = id
            . length
            . filter (== lbl)
            . map (get Type.boundLabel)
            $ take (fromEnum idx) bs
            
      -- Append this count to the end of the variable label, so we know
      -- exactly which variable we are considering
      let lbl'' | same_lbl_count == 0 = lbl'
                | otherwise = lbl' ++ "[" ++ show (same_lbl_count + 1) ++ "]"
      
      return (showWithArgs lbl'' (map snd xs))
    
  -- Default to the non-monadic instance if we don't need to read
  -- from the environment.
  showM other = 
    return (show other)
      
 
-- The cases when we need the inner term along with its representation,
-- viz. we have @Term' (Term, String)@ rather @Term' String@.
instance Show (Term' (Term, String)) where

  -- Special case for displaying constraints
  show (Case' (_, cse_s) f_alts)
    -- A constraint is a match with only one non-absurd branch
    | [Alt' con bs (_, alt_s)] <- non_absurd_alts =
      let pat_s = intercalate " " ([show con] ++ map show bs) in
      "\nassert " ++ pat_s ++ " <- " ++ cse_s ++ " in " ++ alt_s
    where
    non_absurd_alts :: [Alt' (Term, String)]
    non_absurd_alts = 
      filter (not . isUnr . fst . get altTerm') f_alts

  -- Special case for showing equation pairs
  show (Con' eq_con [(_, left_s), (_, right_s)])
    | Type.isEquation eq_con = 
    "(" ++ left_s ++ " == " ++ right_s ++ ")"
    
  -- Special case for showing /if/ expressions
  show (Case' (_, cse_s) 
          [ Alt' c_true [] (_, true_s)
          , Alt' _ [] (_, false_s) ])
    | c_true == Type.true =
      "\nif " ++ indent cse_s
      ++ "\nthen "++ indent true_s
      ++ "\nelse " ++ indent false_s
      
  -- If these don't work then try the @Show (Term' String)@ instance.
  show term' =
    show (fmap snd term')


instance Show (Term' String) where
  show (Var' x xs) = showWithArgs (show x) xs
  show (Unr' _) = "{UNR}"
  show (Lam' (show -> b) t) =
    "\nfun " ++ b ++ " -> " ++ t
  show (Con' con xs) = 
    showWithArgs (show con) xs
  show (Def' name xs) = 
    showWithArgs (show name) xs
  show (Case' cse_t f_alts) =
      "\ncase " ++ cse_t ++ " of"
    ++ concatMap showAlt f_alts
    ++ "\nend"
    where
    showAlt :: Alt' String -> String
    showAlt (Alt' con bs alt_t) =
      "\n| " ++ pat_s ++ " -> " ++ indent alt_t
      where
      pat_s = showWithArgs (show con) (map show bs)
          
instance Env.Bindings m => ShowM m Context where
  showM = showM . get Context.term

instance Show Context where
  show = show . get Context.term
  
instance Env.Bindings m => ShowM m Constraint where
  showM (Constraint con term) = do
    term_s <- showM term
    return (show con ++ " <- " ++ term_s)
    
instance Show Constraint where
  show = Env.empty . showM
      
instance Env.Bindings m => ShowM m Equation where
  showM (Equals n bs t1 t2) = 
    Env.bindMany bs $ do
      t1' <- liftM (dropWhile (== '\n')) (showM t1)
      t2' <- showM t2
      return 
        $ name_s ++ vars_s ++ t1' ++ "\n=\n" ++ t2'
    where
    free_vars = Indices.free t1 ++ Indices.free t2
    useful_bs = map ((reverse bs `nth`) . enum) (toList free_vars)
    bs_s = intercalate " " (map show useful_bs)
    
    vars_s | bs == [] = ""
           | otherwise = "forall " ++ bs_s ++ " ->\n"
    
    name_s | n == "" = ""
           | otherwise = "prop " ++ n ++ ": "
         
    
instance Show Equation where
  show = Env.empty . showM
    

      
