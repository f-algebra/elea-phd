module Elea.Parser.Haskell
(

)
where

import Elea.Prelude
import Elea.Term
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Errors.Parsing as Err
import qualified Elea.Monad.Env.Class as Env
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified CoreSyn as Hs
import qualified Var as Hs
import qualified Type as Hs
import qualified HscTypes as Hs
import qualified DataCon as Hs
import qualified TyCon as Hs
import qualified GHC as Hs
import qualified DynFlags as Hs
import qualified Module as Hs
import qualified HscMain as Hs
import qualified Coercion as Hs
import qualified TidyPgm as Hs
import qualified Digraph as Hs

data HsEnv 
  = HsEnv { _envTerms :: Map Hs.Var Term
          , _envTypes :: Map Hs.Var Type
          , _envBindings :: [Bind]
          , _envTypeArgs :: [Type] }
          
mkLabels [ ''HsEnv ]

type HaskellM m = (Err.Can m, Defs.Has m) => ReaderT HsEnv m

instance Monad m => Env.Read (ReaderT HsEnv m) where
  bindings = asks (get envBindings)
  
instance Monad m => Env.Write (ReaderT HsEnv m) where
  bindAt at b = id
    . local
    $ modify envTerms (addToMap . map (Indices.liftAt at))
    . modify envBindings addToStack
    where
    addToStack = insertAt (enum at) b
    addToMap = Map.insert (get boundLabel b) (Var at)

  matched _ _ = id


localTypeVars :: ContainsTypes t => 
  [Hs.Var] -> ParserMonad m t -> ParserMonad m (Polymorphic t)
localTypeVars vars run = 
  polymorphicM (map show vars) makePoly
  where
  makePoly types =
    local (set envTypeArgs type_args) run
    where
    type_args =
      Map.fromList (zip names types)

loadModule :: Hs.CoreModule -> HaskellM m ()

lookupTerm :: Hs.Var -> HaskellM m Term
lookupTerm var = do
  mby_t <- asks (Map.lookup var . get envTerms)
  case mby_t of
    Just t -> return t
    _ -> Err.variableNotFound (show var)
    
localBinding :: Hs.Var -> Term -> HaskellM m a -> HaskellM m a
localBinding var term = 
  local (modify envTerms (Map.insert var term))

parseBind :: Hs.Bind Hs.Var -> Bind


parseSimpleType :: Hs.Type -> 

parseExpr :: Hs.Expr -> HaskellM m Term
parseExpr (Hs.Var var) = lookupTerm var 
parseExpr (Hs.Lit lit) = Err.literalNotSupported (show lit)
parseExpr (Hs.Type _) = Err.typeArgsNotSupported
parseExpr (Hs.Cast ht _) = parseExpr ht
parseExpr (Hs.Tick _ ht) = parseExpr ht
parseExpr (Hs.App ht1 ht2) = do
  t1 <- parseExpr ht1
  t2 <- parseExpr ht2
  return (app t1 t2)
parseExpr (Hs.Bind (NonRec x ht1) ht2) = do
  t1 <- parseExpr ht1
  localBinding x t1 (parseExpr ht2)
parseExpr (Hs.Bind (Rec [(f, ht1)]) ht2) = do
  
  where
  f_ty = Hs.mkTyVarTy f 


loadTyCon :: Hs.TyCon -> HaskellM m ()
loadTyCon 
