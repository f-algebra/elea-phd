module Elea.Show 
( 
  KleisliShow (..)
) 
where

import Prelude ()
import Elea.Prelude
import Elea.Type ( Type, FBind, Bind )
import Elea.Term ( Term, FTerm, FAlt )
import GHC.Prim ( Constraint )
import qualified Elea.Term as Term
import qualified Elea.Type as Type
import qualified Elea.Foldable as Fix

-- | A monadic variant of 'show'.
class KleisliShow a where
  type ShowM a m :: Constraint
  showM :: ShowM a m => a -> m String
    
instance KleisliShow Type where
  type ShowM Type m = Type.ReadableEnv m
  
  showM = Term.ignoreFacts . Fix.foldM showF
    where
    showF Type.FSet = 
      return "*"
    showF (Type.FInd (Type.FBind (Just lbl) _) _) =
      return lbl
    showF (Type.FVar x) =
        liftM (fromMaybe (show x) . get Type.boundLabel) 
      $ Type.boundAt x
    showF (Type.FApp s1 s2) = 
      return (showApp s1 s2)
    showF (Type.FFun b s) = 
      return (bs' ++ " -> " ++ s)
      where
      bs = showFBind b
      bs' | '(' /= head bs && ' ' `elem` bs = "(" ++ bs ++ ")"
          | otherwise = bs
      
instance KleisliShow Bind where
  type ShowM Bind m = Type.ReadableEnv m
  
  showM = liftM showFBind . mapM showM . Type.projectBind
          
showFBind :: FBind String -> String
showFBind (Type.FBind Nothing s) = s
showFBind (Type.FBind (Just lbl) s) = 
  "(" ++ lbl ++ ": " ++ s ++ ")"
  
showApp :: String -> String -> String
showApp f x 
  | ' ' `elem` x = f ++ " (" ++ x ++ ")"
  | otherwise = f ++ " " ++ x
  
instance KleisliShow Term where
  type ShowM Term m = Type.ReadableEnv m
  
  showM = Term.ignoreFacts . Fix.foldM showF
    where
    showF Term.FAbsurd =
      return "_|_"
    showF (Term.FVar x) =
        liftM (fromMaybe (show x) . get Type.boundLabel) 
      $ Type.boundAt x
    showF (Term.FApp t1 t2) =
      return (showApp t1 t2)
    showF (Term.FType ty) =
        liftM (\s -> "[" ++ s ++ "]") 
      $ showM ty
    showF (Term.FFix (Type.Bind (Just lbl) _) _) = 
      return lbl
    showF (Term.FLam b t) = do
      b_s <- showM b
      return ("fun " ++ b_s ++ " -> " ++ t)
    showF (Term.FInj n (Type.Ind _ cons)) =
        return
      . fromJust
      . get Type.boundLabel
      $ cons !! fromEnum n
    showF (Term.FCase t ind_ty alts) = do
      alts_s <- 
          liftM (indent 2 . concat)
        $ zipWithM showFAlt [0..] alts
      return ("match " ++ t ++ " with" ++ alts_s)
      where
      showFAlt n (Term.FAlt bs t) = do
        con <- showM (Term.Inj n ind_ty)
        bs_s <- liftM (intercalate " ") (mapM showM bs)
        return ("\n" ++ con ++ " " ++ bs_s ++ " -> " ++ t)

