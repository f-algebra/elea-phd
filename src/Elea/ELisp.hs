module Elea.ELisp
(
  ELisp (..), 
  parse, fromList, fromAtom,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term (..), Alt (..) )
import Text.Parsec.String ( Parser )

import qualified Text.Parsec as Parsec
import qualified Elea.Error as Error
import qualified Elea.Definitions as Defs

data ELisp
  = Atom String
  | List [ELisp]
  deriving ( Eq )
  
fromList :: ELisp -> [ELisp]
fromList (List xs) = xs

fromAtom :: ELisp -> String
fromAtom (Atom x) = x

parse :: Error.Monad m => String -> m ELisp
parse txt = 
  Error.fromEither
  $ fmap collapseArrows
  $ runIdentity
  $ Parsec.runParserT parseEL () "" txt
  where
  parseEL :: Parser ELisp
  parseEL = do
    Parsec.spaces 
    elisp <- (List <$> parseList) <|> (Atom <$> parseAtom)
    Parsec.spaces
    return elisp
    
  parseAtom :: Parser String
  parseAtom = Parsec.many1 (Parsec.noneOf "\t ()") 
  
  parseList :: Parser [ELisp] 
  parseList = do
    Parsec.char '('
    elems <- Parsec.many parseEL
    Parsec.char ')'
    return elems
    
collapseArrows :: ELisp -> ELisp
collapseArrows (Atom x) = Atom x
collapseArrows (List xs) = List (collapse xs)
  where
  collapse :: [ELisp] -> [ELisp]
  collapse [] = []
  collapse (Atom "->" : xs) = [List (collapse xs)]
  collapse (x:xs) = (collapseArrows x):(collapse xs)
  

instance Show ELisp where
  show (Atom a) = a
  show (List xs) =
    "(" ++ intercalate " " (map show xs) ++ ")"

