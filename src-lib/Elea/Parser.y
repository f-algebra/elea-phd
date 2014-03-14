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
import qualified Elea.Monad.Env as Env
import qualified Elea.Foldable as Fold
import qualified Elea.Evaluation as Eval
import qualified Elea.Monad.Definitions.Class as Defs      
import qualified Elea.Monad.Error.Class as Err
import qualified Data.Map as Map

type TypeArgs = [String]

-- A parameterised variable call can have type arguments.
-- > append<list<nat>>
type ParamCall = (String, [RawType])

type ParamName = (String, [String])

-- Inductive data types
type TypeDef = (ParamName, [[String]])

-- Let bindings of terms
type TermDef = (ParamName, RawTerm)

-- Declarations to decide equality between terms
type PropDef = (ParamName, [RawBind], (RawTerm, RawTerm))

data RawProgram 
  = RawProgram  { _programTypes :: [TypeDef]
                , _programTerms :: [TermDef]
                , _programProps :: [PropDef] }

data RawType 
  = TBase ParamCall
  | TFun RawType RawType

data RawBind 
  = TBind { _rawLabel :: String
          , _rawType :: RawType }

data RawTerm
  = TVar ParamCall
  | TApp RawTerm RawTerm
  | TFix [RawBind] RawTerm
  | TLam [RawBind] RawTerm
  | TCon Nat RawType
  | TAbsurd RawType
  | TCase RawTerm [RawAlt]
  | TLet String RawTerm RawTerm
  | TEq RawType
  | TFold RawType
  
data RawAlt
  = TAlt [String] RawTerm
  
data Scope 
  = Scope { _bindMap :: Map String Term
          , _bindStack :: [Bind]
          , _typeArgs :: Map String Type }
  
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
  '<'         { TokenOA }
  '>'         { TokenCA }
  ','         { TokenComma }
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
  prop        { TokenProp }
  all         { TokenAll }
  if          { TokenIf }
  then        { TokenThen }
  else        { TokenElse }
  'fold['     { TokenFold  }
  'eq['       { TokenEqEq  }
  
%right '->'
  
%%

Type :: { RawType }
  : ParamCall                         { TBase $1 }
  | '(' Type ')'                      { $2 }
  | Type '->' Type                    { TFun $1 $3 }
  
Cons :: { [[String]] }
  : Pattern                           { [$1] }
  | Cons '|' Pattern                  { $1 ++ [$3] }
  
TypeDef :: { TypeDef }
  : ind ParamName '=' Cons            { ($2, $4) }
  
Pattern :: { [String] }
  : name                              { [$1] }
  | Pattern name                      { $1 ++ [$2] }
  
Matches :: { [RawAlt] }
  : '|' Match                         { [$2] }
  | Matches '|' Match                 { $1 ++ [$3] }
  
Match :: { RawAlt }
  : Pattern '->' Term                 { TAlt $1 $3 }

Bind :: { [RawBind] }
  : '(' Pattern ':' Type ')'          { map (\n -> TBind n $4) $2 }
  
Bindings :: { [RawBind] }
  : Bind                              { $1 }
  | Bindings Bind                     { $1 ++ $2 }
  
Term :: { RawTerm }
  : ParamCall                         { TVar $1 }
  | '_|_' Type                        { TAbsurd $2 }
  | Term ParamCall                    { TApp $1 (TVar $2) }
  | Term '(' Term ')'                 { TApp $1 $3 }  
  | '(' Term ')'                      { $2 }
  | fun Bindings '->' Term            { TLam $2 $4 }
  | fix Bindings '->' Term            { TFix $2 $4 }
  | let name '=' Term in Term         { TLet $2 $4 $6 }
  | 'eq[' Type ']'                    { TEq $2 }
  | 'fold[' Type ']'                  { TFold $2 }
  | match Term with Matches end       { TCase $2 $4 }
  | if Term then Term else Term       { TCase $2 [ TAlt ["True"] $4
                                                 , TAlt ["False"] $6] }
  
TermDef :: { TermDef }
  : let ParamName '=' Term            { ($2, $4) }
  
TypeArgs :: { [RawType] } 
  : {- empty -}                       { [] }
  | Type                              { [$1] }
  | TypeArgs ',' Type                 { $1 ++ [$3] } 
  
ParamCall :: { ParamCall }
  : name                              { ($1, []) }
  | name '<' TypeArgs '>'             { ($1, $3) }
  
ArgNames :: { [String] }
  : {- empty -}                       { [] }
  | name                              { [$1] }
  | ArgNames ',' name                 { $1 ++ [$3] }
  
ParamName :: { ParamName }
  : name                              { ($1, []) }
  | name '<' ArgNames '>'             { ($1, $3) }
  
Equation :: { (RawTerm, RawTerm) }
  : Term                              { ($1, TVar ("True", [])) }
  | Term '=' Term                     { ($1, $3) }
  
PropDef :: { PropDef }
  : prop ParamName ':' all Bindings '->' Equation   
                                      { ($2, $5, $7) }
  | prop ParamName ':' Equation
                                      { ($2, [], $4) }

Program :: { RawProgram }
  : {- empty -}                       { RawProgram [] [] [] }
  | Program TypeDef                   { modify programTypes (++ [$2]) $1 }
  | Program TermDef                   { modify programTerms (++ [$2]) $1 }
  | Program PropDef                   { modify programProps (++ [$2]) $1 }

{

withEmptyScope :: ReaderT Scope m a -> m a
withEmptyScope = flip runReaderT (Scope mempty mempty mempty)

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

localTypeArgs :: ContainsTypes t => 
  [String] -> ParserMonad m t -> ParserMonad m (Polymorphic t)
localTypeArgs names run = 
  polymorphicM names makePoly
  where
  makePoly types =
    local (set typeArgs type_args) run
    where
    type_args =
      Map.fromList (zip names types)
    
localDef :: MonadReader Scope m => String -> Term -> m a -> m a
localDef name term = id
  . local 
  $ modify bindMap 
  $ Map.insert name term
  
term :: (Err.Can m, Defs.Has m) => String -> m Term
term = id
  . withEmptyScope 
  . liftM Eval.run
  . parseAndCheckTerm 
  . happyTerm 
  . lexer
  
_type :: (Err.Can m, Defs.Has m) => String -> m Type
_type = id
  . withEmptyScope
  . parseRawType
  . happyType 
  . lexer
  
program :: forall m . (Err.Can m, Defs.Has m) 
  => String -> m [Polymorphic Equation]
program text = 
  withEmptyScope $ do
    mapM_ defineType types
    mapM_ defineTerm terms
    mapM parseProp props
  where
  RawProgram types terms props = happyProgram (lexer text)
    
  parseProp :: PropDef -> ParserMonad m (Polymorphic Equation)
  parseProp ((name, ty_args), rbs, (rt1, rt2)) = 
    localTypeArgs ty_args $ do
      bs <- mapM parseRawBind rbs
      t1 <- Env.bindMany bs (parseAndCheckTerm rt1)
      t2 <- Env.bindMany bs (parseAndCheckTerm rt2)
      return (Equals name bs t1 t2)
  
  defineType :: TypeDef -> ParserMonad m ()
  defineType ((ind_name, ty_args), raw_cons) = do
    ind_ty <- localTypeArgs ty_args $ do
      cons <- mapM mkCon raw_cons
      return (Type.Ind ind_name cons)
    Defs.defineType ind_name ind_ty
    mapM_ (defCon ind_ty) [0..length raw_cons - 1]
    where
    mkCon :: [String] -> ParserMonad m (String, [Type.ConArg])
    mkCon (con_name:ty_names) = do
      args <- mapM mkArg ty_names
      return (con_name, args)
      where
      mkArg :: String -> ParserMonad m Type.ConArg
      mkArg arg_name
        | arg_name == ind_name = return Type.IndVar
        | otherwise = liftM ConArg (lookupType (arg_name, []))
        
    defCon :: Polymorphic Ind -> Int -> ParserMonad m ()
    defCon poly_ind n =
      Defs.defineTerm name poly_con 
      where
      poly_con = fmap (\ind -> Con ind (enum n)) poly_ind
      name = head (raw_cons !! n)
  
  defineTerm ((name, ty_args), raw_term) = do
    p_term <- localTypeArgs ty_args (parseAndCheckTerm raw_term)
    Defs.defineTerm name (fmap Eval.run p_term)
    
lookupTerm :: ParamCall -> ParserMonad m Term
lookupTerm (name, raw_ty_args) = do
  mby_bind <- asks (Map.lookup name . get bindMap)
  if length raw_ty_args == 0 && isJust mby_bind
  then return (fromJust mby_bind)
  else do
    ty_args <- mapM parseRawType raw_ty_args
    mby_term <- Defs.lookupTerm name ty_args
    if isJust mby_term
    then return (fromJust mby_term)
    else Err.throw $ "Undefined term: " ++ name
    
lookupType :: ParamCall -> ParserMonad m Type
lookupType (name, raw_ty_args) = do
  mby_ty <- asks (Map.lookup name . get typeArgs)
  if length raw_ty_args == 0 && isJust mby_ty
  then return (fromJust mby_ty)
  else do
    ty_args <- mapM parseRawType raw_ty_args
    mby_ind <- Defs.lookupType name ty_args
    if isJust mby_ind
    then return (Base (fromJust mby_ind))
    else Err.throw $ "Undefined inductive type: " ++ name
    
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
parseRawTerm (TFold raw_ty) = do
  Fun (Base ind) res_ty <- parseRawType raw_ty
  return (buildFold ind res_ty)
parseRawTerm (TEq raw_ty) = do
  Base ind <- parseRawType raw_ty
  return (buildEq ind)
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
    $ Fix emptyInfo (head bs) 
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
  | TokenOA
  | TokenCA
  | TokenComma
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
  | TokenProp
  | TokenAll
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenFold
  | TokenEqEq
  
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
lexer ('_':'|':'_':cs) = TokenAbsurd : lexer cs
lexer (':':cs) = TokenTypeOf : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('[':cs) = TokenOS : lexer cs
lexer (']':cs) = TokenCS : lexer cs
lexer ('<':cs) = TokenOA : lexer cs
lexer ('>':cs) = TokenCA : lexer cs
lexer ('|':cs) = TokenBar : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('\"':cs) = TokenName name : lexer rest
  where
  (name, '\"':rest) = span (/= '\"') cs
lexer ('{':cs) = TokenName ("{" ++ name ++ "}") : lexer rest
  where
  (name, '}':rest) = span (/= '}') cs
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
      ("if", rest) -> TokenIf : lexer rest
      ("then", rest) -> TokenThen : lexer rest
      ("else", rest) -> TokenElse : lexer rest
      ("type", rest) -> TokenType : lexer rest
      ("ind", rest) -> TokenInd : lexer rest
      ("end", rest) -> TokenEnd : lexer rest
      ("prop", rest) -> TokenProp : lexer rest
      ("forall", rest) -> TokenAll : lexer rest
      ("fold", '[':rest) -> TokenFold : lexer rest
      ("eq", '[':rest) -> TokenEqEq : lexer rest
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
  show TokenOA = "<"
  show TokenCA = ">"
  show TokenComma = ","
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
  show TokenIf = "if"
  show TokenThen = "then"
  show TokenElse = "else"
  show TokenFold = "fold["
  show TokenEqEq = "eq["
  show (TokenInj n) = "inj" ++ show n
  
  showList = (++) . intercalate " " . map show
}
