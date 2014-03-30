module Elea.Parser.Haskell
(

)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import qualified Elea.Monad.Error.Class as Err
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
  = HsEnv { envVars :: Map Hs.Var Term
          , envTypes :: Map Hs.Var Type }

type HaskellM m = (Err.Can m, Defs.Has m) => ReaderT HsEnv m

localTypeVars :: ContainsTypes t => 
  [Hs.Var] -> ParserMonad m t -> ParserMonad m (Polymorphic t)
localTypeVars vars run = 
  polymorphicM (map show vars) makePoly
  where
  makePoly types =
    local (set typeArgs type_args) run
    where
    type_args =
      Map.fromList (zip names types)

loadModule :: Hs.CoreModule -> HaskellM m ()

parseBind :: Hs.Bind Hs.Var -> 

parseSimpleType :: Hs.Type -> 

parseExpr :: Hs.Expr -> HaskellM m Term
parseExpr (

loadTyCon :: Hs.TyCon -> HaskellM m ()
loadTyCon 
