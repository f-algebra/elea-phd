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
import GHC.Prim ( Constraint )
import qualified Elea.Env as Env
import qualified Elea.Foldable as Fold

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
  show Absurd' = "_|_"
  show (App' f x) = f' ++ " " ++ x'
    where 
    x' | ' ' `elem` x = "(" ++ x ++ ")"
       | otherwise = x
    f' | "->" `isInfixOf` f = "(" ++ f ++ ")"
       | otherwise = f
  show (Ind' b cons) =
    name
    --"fix " ++ show b ++ " with" ++ cons_s ++ " end"
    where
    cons_s = concatMap ((" | " ++) . show) cons
    name = fromJust (get boundLabel' b)
  show (Pi' (show -> b) t) =
    "pi " ++ b ++ mergePi t
  show (Fix' (show -> b) t) =
    indent $ "\nfix " ++ b ++ " -> " ++ t
  show (Lam' (show -> b) t) =
    indent $ "\nfun " ++ b ++ " -> " ++ t
        
instance KleisliShow Term where
  type ShowM Term m = Env.Readable m
  
  showM = Fold.paraM fshow
    where
    fshow :: Env.Readable m => Term' (String, Term) -> m String
    fshow (Var' x) = do
      depth <- Env.bindingDepth
      if x > depth 
      then return (show x)
      else do
        mby_lbl <- liftM (get boundLabel) (Env.boundAt x)
        return (fromMaybe (show x) mby_lbl)
    fshow (Inj' n (ind_s, ind@(Ind _ cons))) =
      return
    --    . (\t -> t ++ ": " ++ ind_s )
        . fromMaybe "_"
        . get boundLabel
        $ cons !! fromEnum n
    fshow (Case' (t, _) (_, arg_ty) alts) = do
      alts_s <- zipWithM showAlt [0..] alts
      return
        . indent
        $ "\nmatch " ++ t 
    --    ++ ": " ++ ty_s
        ++ " with" ++ concat alts_s 
        ++ "\nend"
      where
      ind_ty@(Ind {}) = leftmost arg_ty
      
      showAlt n (Alt' bs (t, _)) = do
        con <- showM (Inj n ind_ty)
        return $ "\n| " ++ con ++ bs_s ++ " -> " ++ t
        where
        bs_s = flip concatMap bs 
          $ (" " ++) . fromMaybe "_" . get boundLabel'
    fshow other = 
      return . show . fmap fst $ other
      
