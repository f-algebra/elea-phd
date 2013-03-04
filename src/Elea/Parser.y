{
module Elea.Parser 
(
  program, term
)
where

import Prelude ()
import Elea.Prelude
import Elea.Type ( Type, Bind )
import Elea.Term ( Term )
import Elea.Index
import Elea.Show ( showM )
import qualified Elea.Type as Type
import qualified Elea.Term as Term
import qualified Elea.Typing as Typing
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Error as Err
import qualified Data.Map as Map

data RawProgram 
  = Program { _programTerms :: [TermDef] 
            , _programTypes :: [TypeDef] }

type TypeDef = (String, RawType)
type TermDef = (String, RawTerm)

data RawBind 
  = TBind { _rawLabel :: Maybe String
          , _rawType :: RawType }
  
data RawType
  = TyVar String
  | TyApp RawType RawType
  | TyFun RawBind RawType
  | TyInd RawBind [RawBind]
  | TySet

data RawTerm
  = TVar String
  | TApp RawTerm RawTerm
  | TFix [RawBind] RawTerm
  | TLam [RawBind] RawTerm
  | TType RawType
  | TInj Nat RawType
  | TAbsurd
  | TCase RawTerm [RawAlt]
  | TLet String RawTerm RawTerm
  
data RawAlt
  = TAlt [String] RawTerm
  
data Scope 
  = Scope { _bindMap :: Map String (Either Term (Index, Type))
          , _bindStack :: [Bind] }
  
mkLabels [''RawProgram, ''Scope, ''RawBind]
}

%name happyProgram Program
%name happyTerm Term

%tokentype { Token }

%token
  name        { TokenName $$ }
  '|'         { TokenBar }
  ':'         { TokenTypeOf }
  '->'        { TokenArr }
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
  
%right '->'

%%

Bind :: { RawBind }
  : name ':' Type                     { TBind (Just $1) $3 }

Cons :: { [RawBind] }
  : '|' Bind                          { [$2] }
  | Cons '|' Bind                     { $1 ++ [$3] }
  
Type :: { RawType }
  : name                              { TyVar $1 }
  | '*'                               { TySet }
  | Type name                         { TyApp $1 (TyVar $2) }
  | Type '(' Type ')'                 { TyApp $1 $3 }
  | Type '->' Type                    { TyFun (TBind Nothing $1) $3 }
  | '(' Bind ')' '->' Type            { TyFun $2 $5 }
  | '(' Type ')'                      { $2 }
  | fix name with Cons end            { TyInd (TBind (Just $2) TySet) $4 }
  
Pattern :: { [String] }
  : name                              { [$1] }
  | Pattern name                      { $1 ++ [$2] }
  
Matches :: { [RawAlt] }
  : '|' Match                         { [$2] }
  | Matches '|' Match                 { $1 ++ [$3] }
  
Match :: { RawAlt }
  : Pattern '->' Term                 { TAlt $1 $3 }
  
Bindings :: { [RawBind] }
  : '(' Bind ')'                      { [$2] }
  | Bindings '(' Bind ')'             { $1 ++ [$3] }
  
Term :: { RawTerm }
  : name                              { TVar $1 }
  | inj '[' Type ']'                  { TInj $1 $3 }
  | Term name                         { TApp $1 (TVar $2) }
  | Term '(' Term ')'                 { TApp $1 $3 }
  | Term '[' Type ']'                 { TApp $1 (TType $3) }
  | '(' Term ')'                      { $2 }
  | fun Bindings '->' Term            { TLam $2 $4 }
  | fix Bindings '->' Term            { TFix $2 $4 }
  | match Term with Matches end       { TCase $2 $4 }
  | let name '=' Term in Term         { TLet $2 $4 $6 }
  
TypeDef :: { TypeDef }
  : type name '=' Type                { ($2, $4) }
  
TermDef :: { TermDef }
  : let name '=' Term                 { ($2, $4) }
  
Program :: { RawProgram }
  : TypeDef                           { Program [] [$1] }
  | TermDef                           { Program [$1] [] }
  | Program TypeDef                   { modify programTypes (++ [$2]) $1 }
  | Program TermDef                   { modify programTerms (++ [$2]) $1 }
  
{

withEmptyScope :: ReaderT Scope m a -> m a
withEmptyScope = flip runReaderT (Scope mempty mempty)

instance Monad m => Type.Env (ReaderT Scope m) where
  bindAt at b = 
      local
    $ modify bindMap (addToMap . map (liftAt at))
    . modify bindStack (addToStack . map (liftAt at))
    where
    addToStack = 
      insertAt (convertEnum at) (liftAt at b)
      
    addToMap 
      | Type.Bind (Just lbl) ty <- b = 
          Map.insert lbl (Right (at, ty))
      | otherwise = id

instance Err.Monad m => Type.ReadableEnv (ReaderT Scope m) where
  boundAt at = 
      asks
    $ (!! fromEnum at)
    . get bindStack
  
type ParserMonad m a = (Err.Monad m, Defs.Monad m) => ReaderT Scope m a
    
localDef :: MonadReader Scope m => String -> Term -> m a -> m a
localDef name term = 
    local 
  $ modify bindMap 
  $ Map.insert name (Left term)
  
term :: (Err.Monad m, Defs.Monad m) => String -> m Term
term = 
    withEmptyScope 
  . parseRawTerm 
  . happyTerm 
  . lexer
  
program :: (Err.Monad m, Defs.Monad m) => String -> m ()
program text = withEmptyScope $ do
  mapM_ addTypeDef typedefs
  mapM_ addTermDef termdefs
  where
  Program termdefs typedefs = happyProgram (lexer text)

  addTypeDef (name, raw_ty) = do
    ty <- parseRawType raw_ty
    Defs.defineType name ty

  addTermDef (name, raw_term) = do
    term <- parseRawTerm raw_term
    Defs.defineTerm name term
    
lookupTerm :: String -> ParserMonad m Term
lookupTerm name = do
  mby_bind <- asks (Map.lookup name . get bindMap)
  case mby_bind of
    Just (Left term) -> return term
    Just (Right (idx, ty))
      | Type.isKind ty -> 
          Err.throw var_err
      | otherwise ->
          return (Term.Var idx)
    Nothing -> do
      mby_term <- Defs.lookupTerm name
      if (isNothing mby_term)
      then Err.throw undef_err
      else return (fromJust mby_term)
  where
  var_err = "Attempted to use type-variable in term position: " ++ name
  undef_err = "Undefined term: " ++ name

lookupType :: String -> ParserMonad m Type
lookupType name = do
  mby_bind <- asks (Map.lookup name . get bindMap)
  case mby_bind of
    Just (Left term) -> 
      Err.throw var_err
    Just (Right (idx, ty))
      | not (Type.isKind ty) -> 
          Err.throw var_err
      | otherwise ->
          return (Type.Var idx)
    Nothing -> do
      mby_type <- Defs.lookupType name
      if (isNothing mby_type)
      then Err.throw undef_err
      else return (fromJust mby_type)
  where
  var_err = "Attempted to use term-variable in type position: " ++ name
  undef_err = "Undefined type: " ++ name
      
parseRawTerm :: RawTerm -> ParserMonad m Term
parseRawTerm TAbsurd = return Term.Absurd
parseRawTerm (TVar var) = lookupTerm var
parseRawTerm (TInj n rty) = do
  ty <- parseRawType rty
  case ty of
    Type.Ind _ cons 
      | fromEnum n >= length cons -> do
          ty_s <- showM ty
          Err.throw 
            $ "Trying to inject into constructor " ++ show n
            ++ " of [" ++ ty_s ++ "] which only has " 
            ++ show (length cons) ++ " constructors."
      | otherwise -> 
          return (Term.Inj n ty)
    _ -> do
      ty_s <- showM ty
      Err.throw 
        $ "The argument type of an inj [" ++ ty_s 
        ++ "] must be an inductive type."
parseRawTerm (TType rty) = do
  ty <- parseRawType rty
  return (Term.Type ty)
parseRawTerm (TApp rt1 rt2) = do
  t1 <- parseRawTerm rt1 
  t2 <- parseRawTerm rt2
  return (Term.App t1 t2)
parseRawTerm (TFix rbs rt) = do
  bs <- parseRawBinds rbs
  t <- Type.bindMany bs (parseRawTerm rt)
  return 
    $ Term.Fix (head bs) 
    $ Term.unflattenLam (tail bs) t 
parseRawTerm (TLam rbs rt) = do
  bs <- parseRawBinds rbs
  t <- Type.bindMany bs (parseRawTerm rt)
  return (Term.unflattenLam bs t)
parseRawTerm (TLet name rt1 rt2) = do
  t1 <- parseRawTerm rt1
  localDef name t1 (parseRawTerm rt2)
parseRawTerm (TCase rt ralts) = do
  t <- parseRawTerm rt
  ind_ty <- Typing.typeOf t
  alts <- mapM (parseRawAlt ind_ty) ralts
  return (Term.Case t ind_ty alts)
  where
  parseRawAlt ind_ty raw_alt = do
    t <- Type.bindMany var_bs (parseRawTerm rt)
    return (Term.Alt var_bs t)
    where
    TAlt (con_lbl:var_lbls) rt = raw_alt
    cons = Type.unfoldInd ind_ty

    -- Find the type of this particular constructor from looking it up
    -- by name in the description of the inductive type.
    Just this_con = find ((== Just con_lbl) . get Type.boundLabel) cons

    -- The bindings for this constructor are the arguments for the type
    con_bs = (fst . Type.flattenFun . get Type.boundType) this_con
    
    -- The new variable bindings are the constructor type bindings, 
    -- renamed to the labels given in the pattern match
    var_bs = zipWith (set Type.boundLabel . Just) var_lbls con_bs
    
parseRawType :: RawType -> ParserMonad m Type
parseRawType (TyVar var) = lookupType var
parseRawType TySet = return Type.Set
parseRawType (TyApp rt1 rt2) = do
  t1 <- parseRawType rt1
  t2 <- parseRawType rt2
  return 
    . Type.reduce
    $ Type.App t1 t2
parseRawType (TyFun rb rty) = do
  b <- parseRawBind rb
  ty <- Type.bind b (parseRawType rty)
  return (Type.Fun b ty)
parseRawType (TyInd rb rcons) = do
  b <- parseRawBind rb
  cons <- Type.bind b (mapM parseRawBind rcons)
  return (Type.Ind b cons)
  
parseRawBinds :: [RawBind] -> ParserMonad m [Bind]
parseRawBinds [] = return []
parseRawBinds (rb:rbs) = do
  b <- parseRawBind rb
  liftM (b:)
    $ Type.bind b
    $ parseRawBinds rbs

parseRawBind :: RawBind -> ParserMonad m Bind
parseRawBind (TBind label raw_ty) = do
  ty <- parseRawType raw_ty
  return (Type.Bind label ty)

data Token
  = TokenBar
  | TokenName String
  | TokenTypeOf
  | TokenSet
  | TokenArr
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
  | TokenType
  | TokenEnd
  | TokenInj Nat
  
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
      ("end", rest) -> TokenEnd : lexer rest
      (name, rest) -> TokenName name : lexer rest
lexer cs = error $ "Unrecognized symbol " ++ take 1 cs

instance Show Token where
  show TokenBar = "|"
  show (TokenName x) = x
  show TokenTypeOf = ":"
  show TokenSet = "*"
  show TokenArr = "->"
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
  show TokenType = "type"
  show TokenEnd = "end"
  show (TokenInj n) = "inj" ++ show n
  
  showList = (++) . intercalate " " . map show
}
