module Elea.Show 
( 
  ShowM (..)
) 
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Context ( Context )
import qualified Elea.Env as Env
import qualified Elea.Type as Type
import qualified Elea.Context as Context
import qualified Elea.Foldable as Fold
import qualified Data.Map as Map

-- | A monadic variant of 'show'.
class Monad m => ShowM m a where
  showM :: a -> m String
    
instance Show Term where
  show = flip runReader ([] :: [Bind]) . showM

instance Show (Term' String) where
  show (Absurd' ty) = "_|_ " ++ show ty
  show (App' f xs) = f' ++ " " ++ xs'
    where 
    xs' = intercalate " " (map showArg xs)
      where
      showArg x 
        | ' ' `elem` x = "(" ++ x ++ ")"
        | otherwise = x
    f' | "->" `isInfixOf` f || "end" `isInfixOf` f = "(" ++ f ++ ")"
       | otherwise = f
  show (Fix' (show -> b) t) =
    indent $ "\nfix " ++ b ++ " -> " ++ t
  show (Lam' (show -> b) t) =
    indent $ "\nfun " ++ b ++ " -> " ++ t
  show (Con' (Type.Ind _ cons) idx) =
    fst (cons !! fromEnum idx)
  show (Case' (Type.unfold -> cons) cse_t f_alts) = id 
     . indent
     $ "match " ++ cse_t ++ " with"
    ++ concat alts_s
    ++ "\nend"
    where
    alts_s = zipWith showAlt cons f_alts
    
    showAlt :: Bind -> Alt' String -> String
    showAlt (Bind con_name _) (Alt' alt_bs alt_t) =
      "\n| " ++ pat_s ++ " -> " ++ alt_t
      where
      pat_s = intercalate " " ([con_name] ++ map show alt_bs)
    
instance Env.Readable m => ShowM m Term where
  showM = Fold.paraM fshow
    where
    fshow :: Term' (String, Term) -> m String
    fshow (Var' idx) = do
      bs <- Env.bindings
      if fromEnum idx >= length bs
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

    fshow other = 
      return . show . fmap fst $ other
      
instance Env.Readable m => ShowM m Context where
  showM = showM . Context.toLambda
  
instance Show Context where
  show = show . Context.toLambda
  
instance ShowM m a => ShowM m (a, a) where
  showM (x, y) = do
    sx <- showM x
    sy <- showM y
    return ("(" ++ sx ++ ", " ++ sy ++ ")")
    
instance ShowM m a => ShowM m [a] where
  showM xs = do
    sxs <- mapM showM xs
    return 
      $ "[" ++ intercalate ", " sxs ++ "]"
 
instance ShowM m a => ShowM m (Map a a) where
  showM = showM . Map.toList
      
