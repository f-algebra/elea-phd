{
module Elea.Parser 
(
  program, term, _type
)
where

import Prelude ()
import Elea.Prelude
import Elea.Type
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Index as Indices
import qualified Elea.Types as Type
import qualified Elea.Env as Env
import qualified Elea.Foldable as Fold
import qualified Elea.Simplifier as Simp
import qualified Elea.Monad.Definitions as Defs      
import qualified Elea.Monad.Error as Err
import qualified Data.Map as Map

-- Inductive data types
type TypeDef = (String, [[String]])

-- Let bindings of terms
type TermDef = (String, RawTerm)

-- "show" declarations to decide equality between terms
type PropDef = (String, [RawBind], (RawTerm, RawTerm))

data RawProgram 
  = RawProgram  { _programTypes :: [TypeDef]
                , _programTerms :: [TermDef]
                , _programProps :: [PropDef] }

data RawType 
  = TBase String
  | TFun RawType RawType

data RawBind 
  = TBind { _rawLabel :: String
          , _rawType :: RawType }

data RawTerm
  = TVar String
  | TApp RawTerm RawTerm
  | TFix [RawBind] RawTerm
  | TLam [RawBind] RawTerm
  | TCon Nat RawType
  | TAbsurd RawType
  | TCase RawTerm [RawAlt]
  | TLet String RawTerm RawTerm
  
data RawAlt
  = TAlt [String] RawTerm
  
data Scope 
  = Scope { _bindMap :: Map String Term
          , _bindStack :: [Bind] }
  
mkLabels [''Scope, ''RawBind, ''RawProgram]
}

%name happyProgram Program
%name happyTerm Term
%name happyType Type

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
  '_|_'       { TokenAbsurd }
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
  prop        { TokenProp }
  all         { TokenAll }
  
%right '->'
  
%%

Type :: { RawType }
  : name                              { TBase $1 }
  | '(' Type ')'                      { $2 }
  | Type '->' Type                    { TFun $1 $3 }
  
Cons :: { [[String]] }
  : Pattern                           { [$1] }
  | Cons '|' Pattern                  { $1 ++ [$3] }
  
TypeDef :: { TypeDef }
  : ind name '=' Cons                 { ($2, $4) }
  
Pattern :: { [String] }
  : name                              { [$1] }
  | Pattern name                      { $1 ++ [$2] }
  
Matches :: { [RawAlt] }
  : '|' Match                         { [$2] }
  | Matches '|' Match                 { $1 ++ [$3] }
  
Match :: { RawAlt }
  : Pattern '->' Term                 { TAlt $1 $3 }

Bind :: { RawBind }
  : '(' name ':' Type ')'             { TBind $2 $4 }
  
Bindings :: { [RawBind] }
  : Bind                              { [$1] }
  | Bindings Bind                     { $1 ++ [$2] }
  
Term :: { RawTerm }
  : name                              { TVar $1 }
  | '_|_' Type                        { TAbsurd $2 }
  | Term name                         { TApp $1 (TVar $2) }
  | Term '(' Term ')'                 { TApp $1 $3 }  
  | '(' Term ')'                      { $2 }
  | fun Bindings '->' Term            { TLam $2 $4 }
  | fix Bindings '->' Term            { TFix $2 $4 }
  | let name '=' Term in Term         { TLet $2 $4 $6 }
  | match Term with Matches end       { TCase $2 $4 }
  
TermDef :: { TermDef }
  : let name '=' Term                 { ($2, $4) }
  
Equation :: { (RawTerm, RawTerm) }
  : Term                              { ($1, TVar "True") }
  | Term '=' Term                     { ($1, $3) }
  
PropDef :: { PropDef }
  : prop name ':' all Bindings '->' Equation   
                                      { ($2, $5, $7) }
  | prop name ':' Equation
                                      { ($2, [], $4) }

Program :: { RawProgram }
  : TypeDef                           { RawProgram [$1] [] [] }
  | TermDef                           { RawProgram [] [$1] [] }
  | PropDef                           { RawProgram [] [] [$1] }
  | Program TypeDef                   { modify programTypes (++ [$2]) $1 }
  | Program TermDef                   { modify programTerms (++ [$2]) $1 }
  | Program PropDef                   { modify programProps (++ [$2]) $1 }

{

withEmptyScope :: ReaderT Scope m a -> m a
withEmptyScope = flip runReaderT (Scope mempty mempty)

instance Monad m => Env.Write (ReaderT Scope m) where
  bindAt at b = 
      local
    $ modify bindMap (addToMap . map (Indices.liftAt at))
    . modify bindStack addToStack
    where
    addToStack = insertAt (enum at) b
    addToMap = Map.insert (get boundLabel b) (Var at)
    
  matched _ _ = id
  
instance Err.Can m => Env.Read (ReaderT Scope m) where
  bindings = asks (get bindStack)
  
type ParserMonad m a = (Err.Can m, Defs.Has m) => ReaderT Scope m a

simplify :: Defs.Has m => Term -> m Term
simplify = Env.emptyT . Simp.run
    
localDef :: MonadReader Scope m => String -> Term -> m a -> m a
localDef name term = id
  . local 
  $ modify bindMap 
  $ Map.insert name term
  
term :: (Err.Can m, Defs.Has m) => String -> m Term
term = id
  . withEmptyScope 
  . (>>= simplify)
  . parseAndCheckTerm 
  . happyTerm 
  . lexer
  
_type :: (Err.Can m, Defs.Has m) => String -> m Type
_type = id
  . withEmptyScope
  . parseRawType
  . happyType 
  . lexer
  
program :: forall m . (Err.Can m, Defs.Has m) => String -> m [Equation]
program text = 
  withEmptyScope $ do
    mapM_ defineType types
    mapM_ defineTerm terms
    mapM parseProp props
  where
  RawProgram types terms props = happyProgram (lexer text)
    
  parseProp :: PropDef -> ParserMonad m Equation
  parseProp (name, rbs, (rt1, rt2)) = do
    bs <- mapM parseRawBind rbs
    t1 <- Env.bindMany bs (parseAndCheckTerm rt1)
    t2 <- Env.bindMany bs (parseAndCheckTerm rt2)
    return (Equals name bs t1 t2)
  
  defineType :: TypeDef -> ParserMonad m ()
  defineType (ind_name, raw_cons) = do
    cons <- mapM mkCon raw_cons
    let ind_ty = Type.Ind ind_name cons
    Defs.defineType ind_name (Type.Base ind_ty)
    mapM_ (defCon ind_ty) [0..length cons - 1]
    where
    mkCon :: [String] -> ParserMonad m (String, [Type.ConArg])
    mkCon (con_name:ty_names) = do
      args <- mapM mkArg ty_names
      return (con_name, args)
      where
      mkArg :: String -> ParserMonad m Type.ConArg
      mkArg arg_name
        | arg_name == ind_name = return Type.IndVar
        | otherwise = liftM Type.ConArg (lookupType arg_name)
        
    defCon :: Type.Ind -> Int -> ParserMonad m ()
    defCon ind_ty n =
      Defs.defineTerm name (Con ind_ty (enum n))
      where
      cons = Type.unfold ind_ty
      name = get Type.boundLabel (cons !! n)
  
  defineTerm (name, raw_term) = do
    term <- parseAndCheckTerm raw_term
    term' <- simplify term
    Defs.defineTerm name term'
    
lookupTerm :: String -> ParserMonad m Term
lookupTerm name = do
  mby_bind <- asks (Map.lookup name . get bindMap)
  if isJust mby_bind
  then return (fromJust mby_bind)
  else do
    mby_term <- Defs.lookupTerm name
    if isJust mby_term
    then return (fromJust mby_term)
    else Err.throw $ "Undefined term: " ++ name
    
lookupType :: String -> ParserMonad m Type
lookupType name = do
  mby_ty <- Defs.lookupType name
  if isJust mby_ty
  then return (fromJust mby_ty)
  else Err.throw $ "Undefind type: " ++ name
    
parseAndCheckTerm :: RawTerm -> ParserMonad m Term
parseAndCheckTerm = 
  Err.check Type.check . parseRawTerm
  
parseRawType :: RawType -> ParserMonad m Type
parseRawType (TBase name) = 
  lookupType name
parseRawType (TFun t1 t2) = 
  return Type.Fun `ap` parseRawType t1 `ap` parseRawType t2

parseRawBind :: RawBind -> ParserMonad m Bind
parseRawBind (TBind label raw_ty) = do
  ty <- parseRawType raw_ty
  return (Bind label ty)

parseRawTerm :: RawTerm -> ParserMonad m Term
parseRawTerm (TVar var) = 
  lookupTerm var
parseRawTerm (TAbsurd rty) = do
  ty <- parseRawType rty
  return (Absurd ty)
parseRawTerm (TApp rt1 rt2) = do
  t1 <- parseRawTerm rt1 
  t2 <- parseRawTerm rt2
  return (app t1 [t2])
parseRawTerm (TFix rbs rt) = do
  bs <- mapM parseRawBind rbs
  t <- Env.bindMany bs (parseRawTerm rt)
  return 
    $ Fix mempty (head bs) 
    $ unflattenLam (tail bs) t 
parseRawTerm (TLam rbs rt) = do
  bs <- mapM parseRawBind rbs
  t <- Env.bindMany bs (parseRawTerm rt)
  return (unflattenLam bs t)
parseRawTerm (TLet name rt1 rt2) = do
  t1 <- parseRawTerm rt1
  localDef name t1 (parseRawTerm rt2)
parseRawTerm (TCase rt ralts) = do
  t <- parseRawTerm rt
  ind_ty <- Type.get t
  alts <- mapM (parseRawAlt ind_ty) ralts
  return (Case (Type.inductiveType ind_ty) t alts)
  where
  parseRawAlt ind_ty (TAlt (con_lbl:var_lbls) ralt_t)
    | not (Type.isInd ind_ty) =
      Err.throw 
        $ "Pattern matching over non inductive type [" ++ show ind_ty ++ "]"
        
    | isNothing mby_this_con =
      Err.throw 
        $ "Invalid constructor \"" ++ con_lbl 
        ++ "\" for type [" ++ show ind_ty ++ "]"
        
    | otherwise = do
      t <- Env.bindMany var_bs (parseRawTerm ralt_t)
      return (Alt var_bs t)
    where
    cons = Type.unfold (Type.inductiveType ind_ty)

    -- Find the type of this particular constructor from looking it up
    -- by name in the description of the inductive type.
    mby_this_con = find ((== con_lbl) . get boundLabel) cons
    Just this_con = mby_this_con
    
    -- The bindings for this constructor are the arguments for the type
    con_tys = (init . Type.flatten . get boundType) this_con
    var_bs = zipWith Bind var_lbls con_tys

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
  | TokenAbsurd
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
  | TokenProp
  | TokenAll
  
happyError :: [Token] -> a
happyError tokens = error $ "Parse error\n" ++ (show tokens)

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c `elem` "'_[]"
  
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
lexer ('_':'|':'_':cs) = TokenAbsurd : lexer cs
lexer (':':cs) = TokenTypeOf : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('|':cs) = TokenBar : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('\"':cs) = TokenName name : lexer rest
  where
  (name, '\"':rest) = span (not . (== '\"')) cs
lexer ('{':cs) = TokenName ("{" ++ name ++ "}") : lexer rest
  where
  (name, '}':rest) = span (not . (== '}')) cs
lexer (c:cs) 
  | isSpace c = lexer cs
  | isNameChar c = lexVar (c : cs)
  where
  lexVar cs =
    case span isNameChar cs of
      ("fun", rest) -> TokenFun : lexer rest
      ("match", rest) -> TokenMatch : lexer rest
      ("with", rest) -> TokenWith : lexer rest
      ("let", rest) -> TokenLet : lexer rest
      ("fix", rest) -> TokenFix : lexer rest
      ("in", rest) -> TokenIn : lexer rest
      ("type", rest) -> TokenType : lexer rest
      ("ind", rest) -> TokenInd : lexer rest
      ("end", rest) -> TokenEnd : lexer rest
      ("prop", rest) -> TokenProp : lexer rest
      ("forall", rest) -> TokenAll : lexer rest
      (name, rest) -> TokenName name : lexer rest
lexer cs = error $ "Unrecognized symbol " ++ take 1 cs

instance Show Token where
  show TokenBar = "|"
  show (TokenName x) = x
  show TokenTypeOf = ":"
  show TokenArr = "->"
  show TokenEq = "="
  show TokenAbsurd = "_|_"
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
  show TokenProp = "prop"
  show TokenAll = "forall"
  show (TokenInj n) = "inj" ++ show n
  
  showList = (++) . intercalate " " . map show
}
