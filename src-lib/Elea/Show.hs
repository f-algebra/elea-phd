-- | The display code for terms and the like. Very hacky and full of commented
-- out bits. But show being a monadic paramorphism over terms is kinda cool.
module Elea.Show 
( 
  ShowM (..)
) 
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Type
import Elea.Context ( Context )
import qualified Elea.Env as Env
import qualified Elea.Type as Type
import qualified Elea.Context as Context
import qualified Elea.Foldable as Fold
import qualified Elea.Definitions as Defs
import qualified Data.Map as Map

-- | A monadic variant of 'show'.
class Monad m => ShowM m a where
  showM :: a -> m String
    
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
  
instance Show (Term' (String, Term)) where
  -- Special case for displaying constraints
  show (Case' (Type.unfold -> cons) (cse_t, _) f_alts)
    -- A constraint is a match with only one non-absurd branch
    | [(Bind con_name _, Alt' bs (alt_t, _))] <- non_absurd_alts =
      let pat_s = intercalate " " ([con_name] ++ map show bs) in
      "\nassert " ++ pat_s ++ " <- " ++ cse_t ++ " in " ++ alt_t
    where                              
    non_absurd_alts :: [(Bind, Alt' (String, Term))]
    non_absurd_alts = id
      . filter (not . isAbsurd . snd . get altInner' . snd) 
      $ zip cons f_alts
      
      
  -- Special case for showing equation pairs
  show (App' (_, Con (Ind _ [("==", _)]) 0) [(left_t, _), (right_t, _)]) = 
    "(" ++ left_t ++ " == " ++ right_t ++ ")"
    
  -- Special case for showing /if/ expressions
  show (Case' (Ind "bool" _) (cse_t, _) 
          [Alt' [] (true_t, _), Alt' [] (false_t, _)]) =
    "\nif " ++ indent cse_t 
    ++ "\nthen "++ indent true_t 
    ++ "\nelse " ++ indent false_t
      
  -- If these don't work then try the @Show (Term' String)@ instance.
  show term' =
    show (fmap fst term')


instance Show (Term' String) where
  show (Absurd' ty) = "_|_ " ++ show ty
  show (App' f xs) = f' ++ " " ++ xs'
    where 
    xs' = intercalate " " (map bracketIfNeeded xs)
    f' | "->" `isInfixOf` f || "end" `isInfixOf` f = "(" ++ f ++ ")"
       | otherwise = f
  show (Fix' _ (show -> b) t) =
    "\nfix " ++ b ++ " -> " ++ indent t
  show (Lam' (show -> b) t) =
    "\nfun " ++ b ++ " -> " ++ t
  show (Con' (Type.Ind _ cons) idx) = id
    . assert (length cons > idx)
    $ fst (cons !! fromEnum idx)
  show (Case' (Type.unfold -> cons) cse_t f_alts) =
      "\nmatch " ++ cse_t ++ " with"
    ++ concat alts_s
    ++ "\nend"
    where
    alts_s = zipWith showAlt cons f_alts
    
    showAlt :: Bind -> Alt' String -> String
    showAlt (Bind con_name _) (Alt' alt_bs alt_t) =
      "\n| " ++ pat_s ++ " -> " ++ indent alt_t
      where
      pat_s = intercalate " " ([con_name] ++ map show alt_bs)
      
instance (Env.Read m, Defs.Read m) => ShowM m (Term' (String, Term)) where
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
            . map (get Type.boundLabel)
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
        inf <- info
        return ("$" ++ name ++ inf ++ args_s) 
        
      Nothing -> 
        -- If we can't find an alias, then default to the normal show instance
        return (show term')
    where
    info
      | Fix' inf@(FixInfo ms) _ _ <- term'
      , not (null ms)
      , False = do
          ms_s <- showM inf 
          return (" " ++ ms_s)
      | otherwise = return ""
          
instance (Env.Read m, Defs.Read m) => ShowM m Context where
  showM = showM . get Context.term
    
instance Show FixInfo where
  show (FixInfo ms) = show ms
  
instance (Env.Read m, Defs.Read m) => ShowM m FixInfo where
  showM (FixInfo ms) = do
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
  showM (Constraint term (Type.Ind _ cons) con_n) = do
    term_s <- showM term
    return (con_name ++ " <- " ++ term_s)
    where
    con_name = fst (cons !! enum con_n)
    
instance Show Constraint where
  show = Defs.readEmpty . Env.emptyT . showM
  
instance (ShowM m a, ShowM m b) => ShowM m (a, b) where
  showM (x, y) = do
    sx <- showM x
    sy <- showM y
    return ("(" ++ sx ++ ", " ++ sy ++ ")")
    
instance ShowM m a => ShowM m [a] where
  showM xs = do
    sxs <- mapM showM xs
    return 
      $ "[" ++ intercalate ", " sxs ++ "]"
      
instance (Env.Read m, Defs.Read m) => ShowM m Equation where
  showM (Equals n bs t1 t2) = 
    Env.bindMany bs $ do
      t1' <- showM t1
      t2' <- showM t2
      return 
        $ "prop " ++ n 
        ++ ": forall " ++ bs' ++ " -> "
        ++ t1' ++ " = " ++ t2'
    where
    bs' = intercalate " " (map show bs)
    
instance Show Equation where
  show = Defs.readEmpty . Env.emptyT . showM
    
instance ShowM m a => ShowM m (Set a) where
  showM xs = do
    xs_s <- showM (toList xs)
    return ("Set" ++ xs_s)
 
instance ShowM m a => ShowM m (Map a a) where
  showM = showM . Map.toList
      
