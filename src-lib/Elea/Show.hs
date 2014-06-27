-- | The display code for terms and the like. Very hacky and full of commented
-- out bits. But show being a monadic paramorphism over terms is kinda cool.
module Elea.Show 
( 
  module Elea.Show.Class
) 
where

import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Type hiding ( get )
import Elea.Context ( Context )
import Elea.Show.Class
import qualified Elea.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Type as Type
import qualified Elea.Context as Context
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Definitions as Defs
import qualified Data.Map as Map

instance Show Term where
  show = emptyEnv . showM
    where
    emptyEnv :: ReaderT [Bind] Defs.DBReader a -> a
    emptyEnv = Defs.readEmpty . flip runReaderT [] 
    
bracketIfNeeded :: String -> String
bracketIfNeeded s 
  | ' ' `elem` s = "(" ++ s ++ ")"
  | otherwise = s
  
instance (Env.Read m, Defs.Read m) => ShowM m Term where
  showM = Fold.paraM showM
  
instance Show (Term' (Term, String)) where
  -- Special case for displaying constraints
  show (Case' (_, cse_t) f_alts)
    -- A constraint is a match with only one non-absurd branch
    | [Alt' con bs (_, alt_t)] <- non_absurd_alts =
      let pat_s = intercalate " " ([show con] ++ map show bs) in
      "\nassert " ++ pat_s ++ " <- " ++ cse_t ++ " in " ++ alt_t
    where                    
    non_absurd_alts :: [Alt' (Term, String)]
    non_absurd_alts =
      filter (not . isUnr . fst . get altInner') f_alts
      
      
  -- Special case for showing equation pairs
  show (App' (Con (Constructor (Ind _ [("==", _)]) 0), _) 
                  [(_, left_t), (_, right_t)]) = 
    "(" ++ left_t ++ " == " ++ right_t ++ ")"
    
  -- Special case for showing /if/ expressions
  show (Case' (_, cse_t) 
          [Alt' con_t [] (_, true_t), Alt' con_f [] (_, false_t)]) 
    | con_t == Type.true && con_f == Type.false =
      "\nif " ++ indent cse_t 
      ++ "\nthen "++ indent true_t 
      ++ "\nelse " ++ indent false_t
      
  -- If these don't work then try the @Show (Term' String)@ instance.
  show term' =
    show (fmap snd term')


instance Show (Term' String) where
  show (Unr' ty) = "UNR"
  show (App' f xs) = f' ++ " " ++ xs'
    where 
    xs' = intercalate " " (map bracketIfNeeded xs)
    f' | "->" `isInfixOf` f || "end" `isInfixOf` f = "(" ++ f ++ ")"
       | otherwise = f
  show (Fix' _ (show -> b) t) =
    "\nfix " ++ b ++ " -> " ++ indent t
  show (Lam' (show -> b) t) =
    "\nfun " ++ b ++ " -> " ++ t
  show (Con' con) = show con
  show (Case' cse_t f_alts) =
      "\nmatch " ++ cse_t ++ " with"
    ++ concatMap show f_alts 
    ++ "\nend"
      
instance Show (Alt' String) where
  show (Alt' con bs t) = 
    "\n| " ++ pat_s ++ " -> " ++ indent t
    where
    pat_s = intercalate " " ([show con] ++ map show bs)
      
instance (Env.Read m, Defs.Read m) => ShowM m (Term' (Term, String)) where
  showM (Var' idx) = do
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
            . map (get Type.bindLabel)
            $ take (fromEnum idx) bs
            
      -- Append this count to the end of the variable label, so we know
      -- exactly which variable we are considering
      if same_lbl_count > 0
      then return (lbl' ++ "[" ++ show (same_lbl_count + 1) ++ "]")
      else return lbl'
      
  {-
  DEBUG CODE
  fshow (Fix' info (show -> b) (t, _)) = do
    inf_s <- showM info
    return 
      $ "\nfix " ++ b ++ " " ++ inf_s ++ " -> " ++ indent t
    -}  
    
  showM term' = do
    -- Attempt to find an alias for this function in our definition database
    mby_name <- Defs.lookupName (Fold.recover term')
    case mby_name of
      Just (name, args) -> do
        args' <- mapM showM args
        let args_s = concatMap ((" " ++) . bracketIfNeeded) args'
        return ("$" ++ name ++ args_s) 
        
      Nothing -> 
        -- If we can't find an alias, then default to the normal show instance
        return (show term')
          
instance (Env.Read m, Defs.Read m) => ShowM m Context where
  showM = showM . get Context.term
    
instance Show FixInfo where
  show (FixInfo ms _ _) = show ms
  
instance (Env.Read m, Defs.Read m) => ShowM m FixInfo where
  showM (FixInfo ms _ _) = do
    ms_s <- mapM showFused ms
    let ms_s' = "[" ++ intercalate ", " ms_s ++ "]"
    return ("fused@" ++ ms_s')
    where
    showFused :: (Set Constraint, Term) -> m String
    showFused (ts, t) = do
      ts_s <- mapM showM (toList ts)
      t_s <- showM t
      return (intercalate ", " ts_s ++ " |- " ++ t_s)
    
instance Show Context where
  show = show . get Context.term
  
instance (Env.Read m, Defs.Read m) => ShowM m Constraint where
  showM (Constraint con term) = do
    term_s <- showM term
    return (show con ++ " <- " ++ term_s)
    
instance Show Constraint where
  show = Defs.readEmpty . Env.emptyT . showM
      
instance (Env.Read m, Defs.Read m) => ShowM m Equation where
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
  show = Defs.readEmpty . Env.emptyT . showM
    
