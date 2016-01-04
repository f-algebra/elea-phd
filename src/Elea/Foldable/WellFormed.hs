module Elea.Foldable.WellFormed 
  ( WellFormed (..), LocalResult (..), assert )
where

import Elea.Prelude hiding ( assert )
import qualified Elea.Prelude as Prelude
import qualified Elea.Foldable as Fold

data LocalResult 
  = LocalPass 
  | LocalFail { failureMsg :: String }
  deriving ( Eq, Ord )

class (Monad m, Runnable m, 
      Fold.Refoldable t, Show t, Foldable (Fold.Base t))
        => WellFormed m t | t -> m where
  -- | All you have to do is implement checkLocal, for any Monad is useful to you, 
  -- as long as it is Runnable.
  checkLocal :: t -> m LocalResult

  failureTrace :: t -> Maybe String
  failureTrace = map concat . run . Fold.para phi
    where
    phi :: forall m t . WellFormed m t 
      => Fold.Base t (t, m (Maybe [String])) 
      -> m (Maybe [String])
    phi base = do
      mby_inner_fail <- firstM (toList (fmap getTrace base))
      case mby_inner_fail of
        Nothing -> do
          mby_local_fail <- checkLocal (Fold.recover base)
          case mby_local_fail of 
            LocalPass -> 
              return Nothing
            LocalFail fail_msg ->
              return (Just [fail_msg])
        Just fail_stack ->  
          return (Just fail_stack)
      where
      getTrace :: (t, m (Maybe [String])) -> m (Maybe [String])
      getTrace (t, m_mby_inner_fail) = do
        mby_inner_fail <- m_mby_inner_fail
        case mby_inner_fail of
          Nothing -> 
            return Nothing
          Just fail_stack -> do
            let new_msg = printf "not well-formed: %s\n\ncaused by " (show t) 
            return (Just (new_msg : fail_stack))

check :: WellFormed m t => t -> Bool
check = isNothing . failureTrace

{-# INLINE assert #-}
assert :: WellFormed m t => t -> a -> a
assert t 
  | not __check__ = id
  | otherwise =
    case failureTrace t of
      Nothing -> id
      Just fail_trace -> error fail_trace
