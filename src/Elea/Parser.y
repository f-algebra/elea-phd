{
module Elea.Parser 
(
  program, term
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Index
import Elea.Show ( showM )
import qualified Elea.Term as Term
import qualified Elea.Typing as Typing
import qualified Elea.Simplifier as Simp
import qualified Elea.Env as Env
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Error as Err
import qualified Data.Map as Map

type TermDef = (String, RawTerm)
type RawProgram = [TermDef]

data RawBind 
  = TBind { _rawLabel :: Maybe String
          , _rawType :: RawTerm }

data RawTerm
  = TVar String
  | TApp RawTerm RawTerm
  | TFix [RawBind] RawTerm
  | TLam [RawBind] RawTerm
  | TInj Nat RawTerm
  | TInd RawBind [RawBind]
  | TAny [RawBind] RawTerm
  | TPi [RawBind] RawTerm
  | TAbsurd
  | TSet
  | TCase RawTerm [RawAlt]
  | TLet String RawTerm RawTerm
  
data RawAlt
  = TAlt [String] RawTerm
  
data Scope 
  = Scope { _bindMap :: Map String (Either Term Index)
          , _bindStack :: [Bind] }
  
mkLabels [''Scope, ''RawBind]
}

%name happyProgram Program
%name happyTerm Term

%tokentype { Token }

%token
  name        { TokenName $$ }
  '|'         { TokenBar }
  ':'         { TokenTypeOf }
  '->'        { TokenArr }
  '=>'        { TokenDArr }
  '*'         { TokenSet }
  '='         { TokenEq }
  '['         { TokenOS }
  ']'         { TokenCS }
  '('         { TokenOP }
  ')'         { TokenCP }
  match       { TokenMatch }
  with        { TokenWith }
  fun         { TokenFun }
  fix         { TokenFix }
  let         { TokenLet }
  in          { TokenIn }
  type        { TokenType }
  end         { TokenEnd }
  inj         { TokenInj $$ }
  ind         { TokenInd }
  any         { TokenAny }
  pi          { TokenPi }
  
%%

NamedBind :: { RawBind }
  : name ':' Term                     { TBind (Just $1) $3 }
  
Bind :: { RawBind }
  : name                              { TBind Nothing (TVar $1) }
  | '(' Term ')'                      { TBind Nothing $2 }
  | '(' NamedBind ')'                 { $2 }
 
Cons :: { [RawBind] }
  : '|' NamedBind                     { [$2] }
  | Cons '|' NamedBind                { $1 ++ [$3] }
  
Pattern :: { [String] }
  : name                              { [$1] }
  | Pattern name                      { $1 ++ [$2] }
  
Matches :: { [RawAlt] }
  : '|' Match                         { [$2] }
  | Matches '|' Match                 { $1 ++ [$3] }
  
Match :: { RawAlt }
  : Pattern '->' Term                 { TAlt $1 $3 }
  
Bindings :: { [RawBind] }
  : Bind                              { [$1] }
  | Bindings Bind                     { $1 ++ [$2] }
  
Term :: { RawTerm }
  : name                              { TVar $1 }
  | '*'                               { TSet }
  | inj Term                          { TInj $1 $2 }
  | Term name                         { TApp $1 (TVar $2) }
  | Term '(' Term ')'                 { TApp $1 $3 }  
  | '(' Term ')'                      { $2 }
  | fun Bindings '->' Term            { TLam $2 $4 }
  | fix Bindings '->' Term            { TFix $2 $4 }
  | any Bindings '->' Term            { TAny $2 $4 }
  | pi Bindings '->' Term             { TPi $2 $4 }
  | let name '=' Term in Term         { TLet $2 $4 $6 }
  | match Term with Matches end       { TCase $2 $4 }
  | ind name with Cons end            { TInd (TBind (Just $2) TSet) $4 }
  | ind NamedBind with Cons end       { TInd $2 $4 }
  

TermDef :: { TermDef }
  : let name '=' Term                 { ($2, $4) }

Program :: { RawProgram }
  : TermDef                           { [$1] }
  | Program TermDef                   { $1 ++ [$2] }

{

withEmptyScope :: ReaderT Scope m a -> m a
withEmptyScope = flip runReaderT (Scope mempty mempty)

instance Monad m => Env.Writable (ReaderT Scope m) where
  bindAt at b = 
      local
    $ modify bindMap (addToMap . map (liftAt at))
    . modify bindStack (addToStack . map (liftAt at))
    where
    addToStack = 
      insertAt (convertEnum at) (liftAt at b)
      
    addToMap 
      | Bind (Just lbl) _ <- b = 
          Map.insert lbl (Right at)
      | otherwise = id
      
  equals _ _ = id

instance Err.Monad m => Env.Readable (ReaderT Scope m) where
  boundAt at = 
      asks
    $ (\bs -> debugNth ("asking " ++ show at ++ " from " ++ show bs) bs (fromEnum at))
    . get bindStack
    
  bindingDepth = 
    asks (toEnum . pred . length . get bindStack)
  
type ParserMonad m a = (Err.Monad m, Defs.Monad m) => ReaderT Scope m a
    
localDef :: MonadReader Scope m => String -> Term -> m a -> m a
localDef name term = 
    local 
  $ modify bindMap 
  $ Map.insert name (Left term)
  
term :: (Err.Monad m, Defs.Monad m) => String -> m Term
term = 
    withEmptyScope 
  . parseAndCheckTerm 
  . happyTerm 
  . lexer
  
program :: (Err.Monad m, Defs.Monad m) => String -> m ()
program = 
    withEmptyScope
  . mapM_ define
  . happyProgram 
  . lexer
  where
  define (name, raw_term) = do
    term <- parseAndCheckTerm raw_term
    Defs.add name term
    
lookupTerm :: String -> ParserMonad m Term
lookupTerm name = do
  mby_bind <- asks (Map.lookup name . get bindMap)
  case mby_bind of
    Just (Left term) -> return term
    Just (Right idx) -> return (Term.Var idx)
    Nothing -> do
      mby_term <- Defs.lookup name
      if (isNothing mby_term)
      then Err.throw $ "Undefined term: " ++ name
      else return (fromJust mby_term)
      
parseAndCheckTerm :: RawTerm -> ParserMonad m Term
parseAndCheckTerm =
    Err.check Typing.check
  . liftM Simp.safe
  . parseRawTerm

parseRawTerm :: RawTerm -> ParserMonad m Term
parseRawTerm TAbsurd = return Absurd
parseRawTerm TSet = return Set
parseRawTerm (TVar var) = lookupTerm var
parseRawTerm (TInj n rty) = do
  ty <- parseRawTerm rty
  return (Inj n ty)
parseRawTerm (TApp rt1 rt2) = do
  t1 <- parseRawTerm rt1 
  t2 <- parseRawTerm rt2
  return (Simp.safe (App t1 t2))
parseRawTerm (TFix rbs rt) = do
  bs <- parseRawBinds rbs
  t <- Env.bindMany bs (parseRawTerm rt)
  return 
    $ Fix (head bs) 
    $ unflattenLam (tail bs) t 
parseRawTerm (TLam rbs rt) = do
  bs <- parseRawBinds rbs
  t <- Env.bindMany bs (parseRawTerm rt)
  return (unflattenLam bs t)
parseRawTerm (TPi rbs rt) = do
  bs <- parseRawBinds rbs
  t <- Env.bindMany bs (parseRawTerm rt)
  return (unflattenPi bs t)
parseRawTerm (TAny rbs rt) = do
  bs <- parseRawBinds rbs
  Env.bindMany bs (parseRawTerm rt)
parseRawTerm (TLet name rt1 rt2) = do
  t1 <- parseRawTerm rt1
  localDef name t1 (parseRawTerm rt2)
parseRawTerm (TInd rb rcons) = do
  b <- parseRawBind rb
  cons <- Env.bind b (mapM parseRawBind rcons)
  return (Ind b cons)
parseRawTerm (TCase rt ralts) = do
  t <- parseRawTerm rt
  ind_ty <- Typing.typeOf t
  alts <- mapM (parseRawAlt ind_ty) ralts
  return (Case t ind_ty alts)
  where
  parseRawAlt ind_ty raw_alt = do
    t <- Env.bindMany var_bs (parseRawTerm rt)
    return (Alt var_bs t)
    where
    TAlt (con_lbl:var_lbls) rt = raw_alt
    cons = unfoldInd ind_ty

    -- Find the type of this particular constructor from looking it up
    -- by name in the description of the inductive type.
    Just this_con = find ((== Just con_lbl) . get boundLabel) cons

    -- The bindings for this constructor are the arguments for the type
    con_bs = (fst . flattenPi . get boundType) this_con
    
    -- The new variable bindings are the constructor type bindings, 
    -- renamed to the labels given in the pattern match
    var_bs = zipWith (set boundLabel . Just) var_lbls con_bs
  
parseRawBinds :: [RawBind] -> ParserMonad m [Bind]
parseRawBinds [] = return []
parseRawBinds (rb:rbs) = do
  b <- parseRawBind rb
  -- Make sure each binding is applied to the latter ones
  liftM (b:)
    $ Env.bind b
    $ parseRawBinds rbs

parseRawBind :: RawBind -> ParserMonad m Bind
parseRawBind (TBind label raw_ty) = do
  ty <- parseRawTerm raw_ty
  return (Bind label ty)

data Token
  = TokenBar
  | TokenName String
  | TokenTypeOf
  | TokenSet
  | TokenArr
  | TokenDArr
  | TokenEq
  | TokenOS
  | TokenCS
  | TokenOP
  | TokenCP
  | TokenMatch
  | TokenWith
  | TokenFun
  | TokenFix
  | TokenLet
  | TokenIn
  | TokenInd
  | TokenType
  | TokenEnd
  | TokenInj Nat
  | TokenAny
  | TokenPi
  
happyError :: [Token] -> a
happyError tokens = error $ "Parse error\n" ++ (show tokens)

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c `elem` "'_"
  
lexer :: String -> [Token]
lexer [] = []
lexer ('/':'*':cs) = lexer (commentEnd cs)
  where
  commentEnd [] = []
  commentEnd ('*':'/':cs) = cs
  commentEnd (c:cs) = commentEnd cs
lexer (' ':cs) = lexer cs
lexer ('\n':cs) = lexer cs
lexer ('-':'>':cs) = TokenArr : lexer cs
lexer ('=':'>':cs) = TokenDArr : lexer cs
lexer ('*':cs) = TokenSet : lexer cs
lexer (':':cs) = TokenTypeOf : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('[':cs) = TokenOS : lexer cs
lexer (']':cs) = TokenCS : lexer cs
lexer ('|':cs) = TokenBar : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer (c:cs) 
  | isSpace c = lexer cs
  | isNameChar c = lexVar (c : cs)
  where
  lexVar cs =
    case span isNameChar cs of
      ('i':'n':'j':num, rest)
        | not (null num) -> TokenInj (toEnum $ read num) : lexer rest
      ("fun", rest) -> TokenFun : lexer rest
      ("match", rest) -> TokenMatch : lexer rest
      ("with", rest) -> TokenWith : lexer rest
      ("let", rest) -> TokenLet : lexer rest
      ("fix", rest) -> TokenFix : lexer rest
      ("in", rest) -> TokenIn : lexer rest
      ("type", rest) -> TokenType : lexer rest
      ("ind", rest) -> TokenInd : lexer rest
      ("end", rest) -> TokenEnd : lexer rest
      ("any", rest) -> TokenAny : lexer rest
      ("pi", rest) -> TokenPi : lexer rest
      (name, rest) -> TokenName name : lexer rest
lexer cs = error $ "Unrecognized symbol " ++ take 1 cs

instance Show Token where
  show TokenBar = "|"
  show (TokenName x) = x
  show TokenTypeOf = ":"
  show TokenSet = "*"
  show TokenArr = "->"
  show TokenDArr = "=>"
  show TokenEq = "="
  show TokenOS = "["
  show TokenCS = "]"
  show TokenOP = "("
  show TokenCP = ")"
  show TokenMatch = "match"
  show TokenWith = "with"
  show TokenFun = "fun"
  show TokenFix = "fix"
  show TokenLet = "let"
  show TokenIn = "in"
  show TokenInd = "ind"
  show TokenType = "type"
  show TokenEnd = "end"
  show (TokenInj n) = "inj" ++ show n
  show TokenAny = "any"
  show TokenPi = "pi"
  
  showList = (++) . intercalate " " . map show
}
