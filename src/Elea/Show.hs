{-# LANGUAGE UndecidableInstances #-}
module Elea.Show 
( 
  KleisliShow (..)
) 
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Type ( Type, Bind', Bind, Type' )
import Elea.Term ( Term, Term', Alt' )
import GHC.Prim ( Constraint )
import Elea.Context ( Context )
import qualified Elea.Term as Term
import qualified Elea.Context as Context
import qualified Elea.Type as Type
import qualified Elea.Foldable as Fix

-- | A monadic variant of 'show'.
class KleisliShow a where
  type ShowM a m :: Constraint
  showM :: ShowM a m => a -> m String
  
instance KleisliShow Type where
  type ShowM Type m = Type.ReadableEnv m
  
  showM = Term.ignoreFacts . Fix.cataM showF
    where
    showF Type.Set' = 
      return "*"
    showF (Type.Var' x) = 
      showBoundAt x  
    showF (Type.Ind' b cons) =
      return name
      --return ("fix " ++ name ++ " with" ++ cons_s ++ " end")
      where
      cons_s = concatMap ((" | " ++) . show) cons
      name = fromJust (get Type.boundLabel' b)
    showF (Type.App' s1 s2) =
      return (showApp s1 s2)
    showF (Type.Fun' (show -> bs) s) =
      return  (bs' ++ " -> " ++ s)
      where
      bs' | "->" `isInfixOf` bs = "(" ++ bs ++ ")"
          | otherwise = bs

instance KleisliShow Bind where
  type ShowM Bind m = Type.ReadableEnv m
  showM = liftM show . mapM showM . Type.projectBind

instance Show (Bind' String) where
  show (Type.Bind' Nothing s) = s
  show (Type.Bind' (Just lbl) s) = 
    "(" ++ lbl ++ ": " ++ s ++ ")"
          
instance Show Type where
  show = flip runReader ([] :: [Bind]) . showM
  
instance Show Bind where
  show = show . fmap show . Type.projectBind
    
instance Show Term where
  show = flip runReader ([] :: [Bind]) . showM
  
showBoundAt :: Type.ReadableEnv m => Index -> m String
showBoundAt x = do
  depth <- Type.bindingDepth
  if x > depth 
  then return (show x)
  else do
    mby_lbl <- liftM (get Type.boundLabel) (Type.boundAt x)
    return (fromMaybe (show x) mby_lbl)
  
showApp :: String -> String -> String
showApp f x = f' ++ " " ++ x'
  where
  x' | ' ' `elem` x = "(" ++ x ++ ")"
     | otherwise = x
  f' | "->" `isInfixOf` f = "(" ++ f ++ ")"
     | otherwise = f
     
-- | Originally this would merge multiple lambdas together into one,
-- but I think things look better if this is disabled.
mergeFun :: String -> String
-- mergeFun (dropWhile (`elem` "\n ") -> 'f':'u':'n':cs) = cs
mergeFun txt = " -> " ++ txt 
  
instance KleisliShow Term where
  type ShowM Term m = Type.ReadableEnv m
  
  showM = Term.ignoreFacts . Fix.cataM showF
    where
    showF (Term.Var' x) =
      showBoundAt x
    showF (Term.Absurd') = 
      return "_|_"
    showF (Term.App' t1 t2) =
      return (showApp t1 t2)
    showF (Term.Type' ty) = do
        liftM (\s -> "[" ++ s ++ "]") 
      $ showM ty
    showF (Term.Fix' b t) = do
      b_s <- showM b
      return 
        . indent $ "\nfix " ++ b_s ++ mergeFun t
    showF (Term.Lam' b t) = do
      b_s <- showM b
      return 
        . indent $ "\nfun " ++ b_s ++ mergeFun t
    showF (Term.Inj' n ty@(Type.Ind _ cons)) = do
      ty_s <- showM ty
      return
    --    . (\t -> t ++ ": " ++ ty_s  )
        . fromMaybe "_"
        . get Type.boundLabel
        $ cons !! fromEnum n
    showF (Term.Case' t arg_ty alts) = do
      ty_s <- showM arg_ty
      alts_s <- zipWithM showAlt [0..] alts
      return
        . indent
        $ "\nmatch " ++ t 
    --    ++ ": " ++ ty_s
        ++ " with" ++ concat alts_s 
        ++ "\nend"
      where
      ind_ty@(Type.Ind {}) = head (Type.flattenApp arg_ty)
      
      showAlt n (Term.Alt' bs t) = do
        con <- showM (Term.Inj n ind_ty)
     --   bs_s <- liftM (intercalate " ") (mapM showM bs)
        return $ "\n| " ++ con ++ bs_s ++ " -> " ++ t
        where
        bs_s = flip concatMap bs 
          $ (" " ++) . fromMaybe "_" . get Type.boundLabel
          
instance Show Context where
  show = show . Context.toLambda
  
instance KleisliShow Context where
  type ShowM Context m = ShowM Term m
  showM = showM . Context.toLambda
      
