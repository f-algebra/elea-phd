-- | The display code for terms and the like. Very hacky and full of commented
-- out bits. But show being a monadic paramorphism over terms is kinda cool.
module Elea.Show 
( 
  module Elea.Show.Class
) 
where

import Elea.Prelude
import Elea.Term.Index
import Elea.Term
import Elea.Type hiding ( get )
import Elea.Show.Class
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Type as Type
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Definitions as Defs
import qualified Data.Map as Map
import qualified Data.Set as Set


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
  -- A constraint is a match with only one non-absurd b{ranch
    | [Alt' con bs (_, alt_t)] <- non_absurd_alts =
      let pat_s = intercalate " " ([show con] ++ map show bs) in
      "\nassert " ++ pat_s ++ " <- " ++ cse_t ++ " in " ++ alt_t
    where                    
    non_absurd_alts :: [Alt' (Term, String)]
    non_absurd_alts =
      filter (not . isBot . fst . get altInner') f_alts
      
    
  -- Special case for showing /if/ expressions
  show (Case' (_, cse_t) 
          [Alt' con_t [] (_, true_t), Alt' con_f [] (_, false_t)]) 
    | Tag.untag con_t == Type.true
    , Tag.untag con_f == Type.false =
      "\nif " ++ indent cse_t 
      ++ "\nthen "++ indent true_t 
      ++ "\nelse " ++ indent false_t
      
  -- If these don't work then try the @Show (Term' String)@ instance.
  show term' =
    show (fmap snd term')


instance Show (Term' String) where
  show (Leq' x y)
    | x == "ff" = "(not " ++ y ++ ")"
    | otherwise = "(" ++ x ++ " =< " ++ y ++ ")"
  show (Seq' x y) = "seq " ++ x ++ " in " ++ y
  show (Bot' (Type.Base ind)) 
    | ind == Type.prop = "tt"
  show (Bot' ty) = "_|_"
  show (App' f xs) = f' ++ " " ++ xs'
    where 
    xs' = intercalate " " (map bracketIfNeeded xs)
    f' | "->" `isInfixOf` f || "end" `isInfixOf` f = "(" ++ f ++ ")"
       | otherwise = f
  show (Fix' inf (show -> b) t) =
    "\nfix " ++ b ++ " -> " ++ indent t
  show (Lam' (show -> b) t) =
    "\nfun " ++ b ++ " -> " ++ t 
  show (Con' con) = show con
  show (Case' cse_t f_alts) =
      "\nmatch " ++ cse_t ++ " with"
    ++ concatMap show f_alts 
    ++ "\nend"
  
instance (Env.Read m, Defs.Read m) => ShowM m FixInfo where
  showM _ = return "" 

instance Show Match where
  show m = show (matchedTo m) ++ " <- " ++ show (matchedTerm m)
  
instance (Env.Read m, Defs.Read m) => ShowM m Constraint where
  showM m = do
    ts <- showM (matchedTerm m)
    ps <- showM (matchedTo m)
    return (ps ++ " <- " ++ ts)
    
instance Env.Read m => ShowM m Index where
  showM idx = do
    Bind n _ <- Env.boundAt idx
    return n
    
    
instance Show (Alt' String) where
  show (Alt' con bs t) = 
    "\n| " ++ pat_s ++ " -> " ++ indent t
    where
    pat_s = intercalate " " ([show con] ++ map show bs)
                                       
instance (Env.Read m, Defs.Read m) => ShowM m (Term' (Term, String)) where
  showM (Var' idx _) = do
    bs <- Env.bindings                   
    if idx >= elength bs
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
  showM (Fix' fix_i fix_b (_, fix_t)) 
    | (not . null . get fixFailedAttempts) fix_i = do
      fix_is <- showM fix_i
      return ("\n" ++ fix_is 
        ++ "\nfix"
        ++ " " ++ show fix_b 
        ++ " -> " ++ indent fix_t)
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
          
        {-
instance (Env.Read m, Defs.Read m) => ShowM m Context where
  showM = showM . get Context.term
    
instance Show Context where
  show = show . get Context.term
  
instance (Env.Read m, Defs.Read m) => ShowM m Constraint where
  showM (Constraint con term) = do
    term_s <- showM term
    return (show con ++ " <- " ++ term_s)
    
instance Show Constraint where
  show = Defs.readEmpty . Env.emptyT . showM
  -}
      
instance (Env.Read m, Defs.Read m) => ShowM m Prop where
  showM (Prop "" t) = showM t
  showM (Prop name t) = do
    ts <- showM t
    return
      $ "prop " ++ name ++ " = " ++ ts
      
instance Show Prop where
  show (Prop "" t) = show t
  show (Prop name t) = 
    "prop " ++ name ++ " = " ++ show t

instance PrintfArg Term where
  formatArg = formatString . show