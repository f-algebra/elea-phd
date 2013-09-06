{-# LANGUAGE UndecidableInstances #-}
module Elea.Show 
( 
  KleisliShow (..)
) 
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Context ( Context )
import GHC.Prim ( Constraint )
import qualified Elea.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Foldable as Fold
import qualified Data.Map as Map

-- | A monadic variant of 'show'.
class KleisliShow a where
  type ShowM a m :: Constraint
  showM :: (Monad m, ShowM a m) => a -> m String

instance KleisliShow Bind where
  type ShowM Bind m = Env.Readable m
  showM = liftM show . mapM showM . projectBind

instance Show (Bind' String) where
  show (Bind' Nothing s) = s
  show (Bind' (Just lbl) s) = 
    "(" ++ lbl ++ ": " ++ s ++ ")"
          
instance Show Bind where
  show = show . fmap show . projectBind
    
instance Show Term where
  show = flip runReader ([] :: [Bind]) . showM

mergePi :: String -> String
mergePi (dropWhile (`elem` "\n ") -> 'p':'i':cs) = cs
mergePi other = " -> " ++ other

instance Show (Term' String) where
  show Set' = "*"
  show Type' = "TYPE"
  show (Absurd' ty) = "_|_ " ++ ty
  show (App' f xs) = f' ++ " " ++ xs'
    where 
    xs' = intercalate " " (map showArg xs)
      where
      showArg x 
        | ' ' `elem` x = "(" ++ x ++ ")"
        | otherwise = x
    f' | "->" `isInfixOf` f || "end" `isInfixOf` f = "(" ++ f ++ ")"
       | otherwise = f
  show (Ind' b cons) =
    name
    --"(ind " ++ show b ++ " with" ++ cons_s ++ " end)"
    where
    cons_s = concatMap ((" | " ++) . show) cons
    name = fromJust (get boundLabel' b)
  show (Pi' (show -> b) t) =
    "pi " ++ b ++ mergePi t
  show (Fix' (FixInfo' inf _ _) (show -> b) t) =
    indent $ "\nfix " {- ++ show inf -} ++ " " ++ b ++ " -> " ++ t
  show (Lam' (show -> b) t) =
    indent $ "\nfun " ++ b ++ " -> " ++ t
        
instance KleisliShow Term where
  type ShowM Term m = Env.Readable m
  
  showM = Fold.paraM fshow
    where
    fshow :: Env.Readable m => Term' (String, Term) -> m String
    fshow (Var' idx) = do
      bs <- Env.bindings
      if n >= length bs
      then return (show idx)
      else do
        mby_lbl <- liftM (get boundLabel) (Env.boundAt idx)
        case mby_lbl of
          Nothing -> return (show idx)
          Just lbl -> do
            let same_lbl_count = id
                  . length
                  . filter (== mby_lbl)
                  . map (get boundLabel)
                  $ take n bs
            if same_lbl_count > 0
            then return (lbl ++ "[" ++ show (same_lbl_count + 1) ++ "]")
            else return lbl
      where
      n = fromEnum idx
      
    fshow (Inj' n (ind_s, ind@(Ind _ cons))) =
      return
  --    . (\t -> t ++ "[: " ++ ind_s ++ "]")
      . fromMaybe "_"
      . get boundLabel
      $ cons !! fromEnum n
    fshow (Case' (t, _) (ty_s, ind_ty@(Ind {})) alts) = do
      alts_s <- zipWithM showAlt [0..] alts
      return
        . indent
        $ "\nmatch " ++ t 
    --    ++ ": " ++ ty_s
        ++ " with" ++ concat alts_s 
        ++ "\nend"
      where
      showAlt n (Alt' bs (t, _)) = do
        con <- showM (Inj n ind_ty)
        return $ "\n| " ++ con ++ bs_s ++ " -> " ++ t
        where
        showB (Bind' (Just lbl) (ty_s, ty)) = 
          Just $ "(" ++ lbl ++ ": " ++ ty_s ++ ")"
        
        bs_s = flip concatMap bs 
          $ (" " ++) . fromMaybe "_" . get boundLabel'
    fshow other = 
      return . show . fmap fst $ other
      
instance KleisliShow Context where
  type ShowM Context m = ShowM Term m
  showM = showM . Context.toLambda
  
instance Show Context where
  show = show . Context.toLambda
  
instance KleisliShow a => KleisliShow (a, a) where
  type ShowM (a, a) m = ShowM a m
  showM (x, y) = do
    sx <- showM x
    sy <- showM y
    return ("(" ++ sx ++ ", " ++ sy ++ ")")
    
instance KleisliShow a => KleisliShow [a] where
  type ShowM [a] m = ShowM a m
  showM xs = do
    sxs <- mapM showM xs
    return 
      $ "[" ++ intercalate ", " sxs ++ "]"
 
instance KleisliShow a => KleisliShow (Map a a) where
  type ShowM (Map a a) m = ShowM a m
  showM = showM . Map.toList
      
