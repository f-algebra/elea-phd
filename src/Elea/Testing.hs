module Elea.Testing 
(
  M,
  test, testWithPrelude,
  assertSimpEq,
  label, list,
  loadFile,
  loadPropertyNamesFromFile,
  term, _type,
  simplified,
  assertTermEq,
  assertBool,
  assertEq,
  localVars,
  recordTimeTaken,
  printTestTimes,
  withEnv,
  module Test.HUnit
) 
where

import Elea.Prelude hiding ( assertEq )
import Elea.Term
import Elea.Type hiding ( get, assertEq )
import Elea.Monad.Fedd ( FeddT )
import Test.HUnit ( Test, assertFailure )
import Control.Exception ( catch, SomeException )
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Ext as Term
import qualified Elea.Monad.Fedd.Include as Fedd
import qualified Elea.Parser.Calculus as Parse
import qualified Elea.Monad.Fedd as Fedd
import qualified Elea.Monad.Env as Env
import qualified Elea.Transform.Simplify as Simp
import qualified Elea.Transform.Fusion as Fusion
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Discovery as Discovery
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Memo.Class as Memo
import qualified Test.HUnit as HUnit
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.IORef as Data
import qualified System.CPUTime as System
import qualified Text.PrettyPrint.Boxes as Box

type M = FeddT IO

{-# NOINLINE testTimes #-}
testTimes :: Data.IORef [(String, Integer)]
testTimes = unsafePerformIO (Data.newIORef [])

test :: String -> M () -> Test
test label = id
  . HUnit.TestLabel label 
  . HUnit.TestCase 
  -- . flip catch (\(e :: SomeException) -> assertFailure (show e))
  -- ^ Not sure why this isn't built in
  . Fedd.evalT

label :: String -> Test -> Test
label = HUnit.TestLabel

list :: [Test] -> Test
list = HUnit.TestList

testWithPrelude :: String -> M () -> Test
testWithPrelude label = test label . (Fedd.loadPrelude >>)

assertBool :: String -> Bool -> M ()
assertBool msg = Trans.lift . HUnit.assertBool msg

assertEq :: (Show a, Eq a) => String -> a -> a -> M ()
assertEq msg x y = Trans.lift (HUnit.assertEqual msg x y)

assertTermEq :: String ->Term -> Term -> M ()
assertTermEq msg (stripTags -> t1) (stripTags -> t2) = do
  let msg' | null msg = ""
           | otherwise = printf "[%s]\n" msg
  let prop_s = printf "%sexpected: %s\nbut got: %s" msg' t1 t2
  assertEq prop_s t1 t2
  
assertSimpEq :: String -> Term -> Term -> M ()
assertSimpEq msg t1 t2 = do
  t1' <- Simp.applyM t1
  t2' <- Simp.applyM t2
  assertTermEq msg t1' t2'

loadFile :: String -> M [Prop]
loadFile file_name = do
  contents <- Trans.lift (readFile file_name) 
  Err.noneM 
    . liftM (map uninterpreted) 
    $ Parse.program contents

loadPropertyNamesFromFile :: String -> IO [String]
loadPropertyNamesFromFile file_name = do
  all_props <- Fedd.evalT $ do
    Fedd.loadPrelude
    loadFile file_name
  return (map (get propName) all_props)

term :: Monad m => String -> m Term
term = return . read

simplified :: String -> Term
simplified = Simp.apply . read

_type :: (Tag.Gen m, Defs.Has m) => String -> m Type
_type = Err.noneM . Parse._type

localVars :: String -> M a -> M a
localVars bs_s run = do
  bs <- Err.noneM (Parse.bindings bs_s)
  Env.bindMany bs $ do
    zipWithM defineBind [0..] (reverse bs)
    run
  where
  defineBind idx b@(Bind lbl _) =
    Defs.defineTerm lbl p_term
    where
    p_term = polymorphic [] (const (Var (enum idx) b))

recordTimeTaken :: MonadIO m => String -> m a -> m a
recordTimeTaken run_name run = do
  time_before <- liftIO System.getCPUTime
  result <- run
  time_after <- liftIO System.getCPUTime
  let time_diff_ms = (time_after - time_before) `div` (10 ^ 9)
  liftIO 
    $ Data.atomicModifyIORef' testTimes 
    $ \msgs -> (msgs ++ [(run_name, time_diff_ms)], ())
  return result

printTestTimes :: IO ()
printTestTimes = do
  msgs <- Data.atomicModifyIORef' testTimes 
            $ \msgs -> ([], msgs)
  let (keys, vals) = unzip msgs
  putStrLn "\n<Test times>"
  Box.printBox (vbox keys Box.<+> vbox (map (printf "%dms") vals))
  where
  vbox = Box.vcat Box.top . map Box.text

withEnv :: String -> [String] -> [Term]
withEnv env_str =
  map (\t -> read (printf "env %s in (%s)" env_str t)) 
