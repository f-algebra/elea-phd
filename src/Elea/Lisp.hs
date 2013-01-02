module Elea.Lisp
(
  Lisp (..), InnerLisp (..),
  parse, isAtom, fromAtom
)
where

import Prelude ()
import Elea.Prelude
import Text.Parsec.String ( Parser )

import qualified Text.Parsec as Parsec
import qualified Data.Map as Map
import qualified Elea.Monad.Error as Error

data Lisp
  = Lisp { notes :: Map String Lisp
         , inner :: InnerLisp }
  deriving ( Eq )
 
data InnerLisp
  = Atom String
  | List [Lisp]
  deriving ( Eq )
  
addNotes :: Map String Lisp -> Lisp -> Lisp
addNotes more (Lisp notes inner) =
  Lisp (notes ++ more) inner
  
isAtom :: Lisp -> Bool
isAtom (Lisp _ (Atom _)) = True
isAtom _ = False

fromAtom :: Lisp -> String
fromAtom (Lisp _ (Atom x)) = x

parse :: Error.Monad m => String -> m Lisp
parse txt = 
  Error.fromEither
  $ runIdentity
  $ Parsec.runParserT parseNakedList () "" txt
  
parseLisp :: Parser Lisp
parseLisp = do
  Parsec.spaces 
  elisp <- parseList <|> parseAtom
  Parsec.spaces
  return elisp
  
parseNotes :: WriterT (Map String Lisp) Parser ()
parseNotes = (<|> return ()) $ do
  (tell =<<) $ lift $ do
    Parsec.char ':'
    name <- parseString
    Parsec.spaces
    lisp <- parseLisp
    Parsec.spaces
    return (Map.singleton name lisp)
  parseNotes
  
parseString :: Parser String
parseString = Parsec.many1 (Parsec.noneOf " :()\t\n") 

parseAtom :: Parser Lisp
parseAtom = (Lisp mempty . Atom) <$> parseString

parseListElems :: WriterT (Map String Lisp) Parser [Lisp] 
parseListElems = do
  lift Parsec.spaces
  parseNotes
  (<|> return []) $ do
    x <- lift parseLisp
    if x == Lisp mempty (Atom "->") 
    then return <$> lift parseNakedList
    else do
      xs <- parseListElems
      return (x:xs)
  
parseNakedList :: Parser Lisp
parseNakedList = do
  (ls, notes) <- runWriterT parseListElems
  Parsec.spaces
  if length ls == 1
  then return $ addNotes notes (head ls)
  else return $ Lisp notes (List ls)
    
parseList :: Parser Lisp
parseList = do
  Parsec.char '('
  list <- parseNakedList
  Parsec.char ')'
  return list
    

instance Show Lisp where
  show (Lisp notes (Atom x)) 
    | Map.null notes = x
    | otherwise = show (Lisp notes (List [Lisp mempty (Atom x)]))
  show (Lisp notes (List ls)) =
    "(" ++ intercalate " " (map show ls ++ notes_s) ++ ")"
    where
    notes_s = map showNote (Map.toList notes)   
    showNote (name, val) = 
      ":" ++ name ++ " " ++ show val

