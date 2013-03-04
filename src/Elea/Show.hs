module Elea.Show 
( 
  KleisliShow (..)
) 
where

import Prelude ()
import Elea.Prelude
import Elea.Type ( Type, Bind', Bind, Type' )
import Elea.Term ( Term, Term', Alt' )
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
  
  showM = Term.ignoreFacts . Fix.cataM showF
    where
    showF (Type.Var' x) =
        liftM (fromMaybe (show x) . get Type.boundLabel) 
      $ Type.boundAt x
    showF ty = 
      return (show ty)

instance KleisliShow Bind where
  type ShowM Bind m = Type.ReadableEnv m
  showM = liftM show . mapM showM . Type.projectBind

instance Show (Type' String) where
  show Type.Set' = "*"
  show (Type.Var' x) = show x
  show (Type.Ind' b cons) = name
   -- "fix " ++ name ++ " with" ++ cons_s ++ " end"
    where
    name = fromJust (get Type.boundLabel' b)
    cons_s = concatMap ((" | " ++) . show) cons
  show (Type.App' s1 s2) =
    showApp s1 s2
  show (Type.Fun' (show -> bs) s) =
    bs' ++ " -> " ++ s
    where
    bs' | "->" `isInfixOf` bs = "(" ++ bs ++ ")"
        | otherwise = bs

instance Show (Bind' String) where
  show (Type.Bind' Nothing s) = s
  show (Type.Bind' (Just lbl) s) = 
    "(" ++ lbl ++ ": " ++ s ++ ")"
    
instance Show (Term' String) where
  show (Term.Var' x) = show x
  show (Term.Absurd') = "_|_"
  show (Term.App' t1 t2) =
    showApp t1 t2
  show (Term.Type' ty) = 
    "[" ++ show ty ++ "]"
  show (Term.Fix' b t) =
    indent $ "\nfix " ++ show b ++ mergeFun t
  show (Term.Lam' b t) =
    indent $ "\nfun " ++ show b ++ mergeFun t
  show (Term.Inj' n (Type.Ind _ cons)) = 
      fromJust
    . get Type.boundLabel
    $ cons !! fromEnum n
  show (Term.Case' t arg_ty alts) =
        indent
      $ "\nmatch " ++ t 
      ++ " with" ++ concat alts_s 
      ++ "\nend"
    where
    alts_s = zipWith showAlt' [0..] alts
    ind_ty@(Type.Ind {}) = head (Type.flattenApp arg_ty)
    
    showAlt' n (Term.Alt' bs t) =
      "\n| " ++ con ++ " " ++ bs_s ++ " -> " ++ t
      where
      con = show (Term.Inj n ind_ty)
      bs_s = intercalate " " 
        $ map (fromJust . get Type.boundLabel) bs
          
instance Show Bind where
  show = show . fmap show . Type.projectBind
    
instance Show Type where
  show = Fix.cata show
  
instance Show Term where
  show = Fix.cata show
  
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
    -- return (show x)
   --   liftM show
        liftM (fromMaybe (show x) . get Type.boundLabel) 
      $ Type.boundAt x
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
    showF t = 
      return (show t)

