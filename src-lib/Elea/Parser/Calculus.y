{
-- | A parser for Elea's raw input calculus.
module Elea.Parser.Calculus
(
  program, term, _type, bindings
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Index as Indices
import qualified Elea.Terms as Term
import qualified Elea.Types as Type
import qualified Elea.Constraint as Constraint
import qualified Elea.Monad.Env as Env
import qualified Elea.Foldable as Fold
import qualified Elea.Simplifier as Simp
import qualified Elea.Evaluation as Eval
import qualified Elea.Monad.Parser.Class as Parser
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Errors.Parsing as Err
import qualified Data.Map as Map

type TypeArgs = [String]

-- A parameterised variable call can have type arguments.
-- > append<list<nat>>
type InstName = (String, [RawType])

type PolyName = (String, [String])

-- Inductive data types
type TypeDef = (PolyName, [[String]])

-- Let bindings of terms
type TermDef = (PolyName, [RawBind], RawType, RawTerm)

-- Declarations to decide equality between terms
type PropDef = (PolyName, [RawBind], (RawTerm, RawTerm))

data RawProgram 
  = RawProgram  { _programTypes :: [TypeDef]
                , _programTerms :: [TermDef]
                , _programProps :: [PropDef] }

data RawType 
  = TyBase InstName
  | TyFun RawType RawType
  | TyTuple [RawType]

data RawBind 
  = TBind { _rawLabel :: String
          , _rawType :: RawType }

data RawTerm
  = TApp InstName [RawTerm]
  | TLam [RawBind] RawTerm
  | TUnr RawType
  | TCase RawTerm [RawAlt]
  | TAssert [String] RawTerm RawTerm
  
data RawAlt
  = TAlt [String] RawTerm
  
data Scope 
  = Scope { _bindMap :: Map String Index
          , _bindStack :: [Bind] }
  
mkLabels [''Scope, ''RawBind, ''RawProgram]
}

%name happyProgram Program
%name happyTerm Term
%name happyType Type
%name happyBindings Bindings

%tokentype { Token }

%token
  name        { TokenName $$ }
  '|'         { TokenBar }
  ':'         { TokenTypeOf }
  '->'        { TokenRArr }
  '<-'        { TokenLArr }
  '=>'        { TokenDRArr }
  '*'         { TokenSet }
  '='         { TokenEq }
  '['         { TokenOS }
  ']'         { TokenCS }
  '('         { TokenOP }
  ')'         { TokenCP }
  '<'         { TokenOA }
  '>'         { TokenCA }
  ','         { TokenComma }
  match       { TokenMatch }
  with        { TokenWith }
  fun         { TokenFun }
  'unr['      { TokenUnr }
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
  assert      { TokenAssert }
  
%%

Type :: { RawType }
  : InstName                          { TyBase $1 }
  | InstName '->' Type                { TyFun (TyBase $1) $3 }
  | '(' Type ')' '->' Type            { TyFun $2 $5 }
  
Cons :: { [[String]] }
  : Pattern                           { [$1] }
  | Cons '|' Pattern                  { $1 ++ [$3] }
  
TypeDef :: { TypeDef }
  : ind PolyName '=' Cons             { ($2, $4) }
  
NameSeq :: { [String] }
  : name                              { [$1] }
  | NameSeq name                      { $1 ++ [$2] }
  
NameList :: { [String] }
  : name                              { [$1] }      
  | NameList ',' name                 { $1 ++ [$3] }
                                        
TermList :: { [RawTerm] }
  : Term                              { [$1] }
  | TermList ',' Term                 { $1 ++ [$3] }
  
TypeList :: { [RawType] } 
  : Type                              { [$1] }
  | TypeList ',' Type                 { $1 ++ [$3] } 
  
Pattern :: { [String] }
  : NameSeq                           { $1 }
  | '(' name ',' NameList ')'         { "tuple":$2:$4 }
  
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
  : InstName Args                     { TApp $1 $2 }
  | '(' Term ')'                      { $2 }
  | fun Bindings '->' Term            { TLam $2 $4 }
  | 'unr[' Type ']'                   { TUnr $2 }
  | match Term with Matches end       { TCase $2 $4 }
  | assert Pattern '<-' Term in Term  { TAssert $2 $4 $6 }
  | if Term then Term else Term       { TCase $2 [ TAlt ["True"] $4
                                                 , TAlt ["False"] $6] }
                                                 
Args :: { [RawTerm] }
  : {- empty -}                       { [] }
  | Args Term                         { $1 ++ [$2] }

TermDef :: { TermDef }
  : let PolyName Bindings ':' Type '=' Term    
                                      { ($2, $3, $5, $7) }
  
InstName :: { InstName }
  : name                              { ($1, []) }
  | name '<' TypeList '>'             { ($1, $3) }
  
PolyName :: { PolyName }
  : name                              { ($1, []) }
  | name '<' NameList '>'             { ($1, $3) }
  
Equation :: { (RawTerm, RawTerm) }
  : Term                              { (TApp ("True", []) [], $1) }
  | Term '=' Term                     { ($1, $3) }
  
PropDef :: { PropDef }
  : prop PolyName ':' all Bindings '->' Equation   
                                      { ($2, $5, $7) }
  | prop PolyName ':' Equation
                                      { ($2, [], $4) }

Program :: { RawProgram }
  : {- empty -}                       { RawProgram [] [] [] }
  | Program TypeDef                   { modify programTypes (++ [$2]) $1 }
  | Program TermDef                   { modify programTerms (++ [$2]) $1 }
  | Program PropDef                   { modify programProps (++ [$2]) $1 }

{

withEmptyScope :: ReaderT Scope m a -> m a
withEmptyScope = flip runReaderT (Scope mempty mempty)

instance Monad m => Env.Write (ReaderT Scope m) where
  bindAt at b = 
      local
    $ modify bindMap (Map.insert (bindLabel b) at . map (Indices.liftAt at))
    . modify bindStack (insertAt (enum at) b)
    
  matched _ _ = id
  
instance Err.Throws m => Env.Bindings (ReaderT Scope m) where
  bindings = asks (get bindStack)
  
instance (Err.Throws m, Parser.State m) => Parser.State (ReaderT Scope m) where
  defineTerm n = lift . Parser.defineTerm n
  defineInd n = lift . Parser.defineInd n
  lookupTerm = lift . Parser.lookupTerm
  lookupInd = lift . Parser.lookupInd
  
type ParserMonad m = (Err.Throws m, Defs.Write m, Parser.State m)
type Parse m a = ParserMonad m => ReaderT Scope m a
  
term :: ParserMonad m => String -> m Term
term str = do
  bs <- Env.bindings
  withEmptyScope
    . liftM Eval.run
    . Env.bindMany (reverse bs)
    . parseAndCheckTerm 
    . happyTerm 
    . lexer
    $ str
  
_type :: ParserMonad m => String -> m Type
_type = id
  . withEmptyScope
  . parseRawType
  . happyType 
  . lexer
  
bindings :: ParserMonad m => String -> m [Bind]
bindings = id
  . withEmptyScope
  . mapM parseRawBind
  . happyBindings
  . lexer
  
program :: forall m . ParserMonad m
  => String -> m [Equation]
program text = 
  withEmptyScope $ do
    mapM_ defineType types
    mapM_ defineTerm terms
    mapM parseProp props
  where
  RawProgram types terms props = happyProgram (lexer text)
    
  parseProp :: PropDef -> Parse m Equation
  parseProp ((name, ty_vars), rbs, (rt1, rt2)) = do
    bs <- mapM parseRawBind rbs
    t1 <- Env.bindMany bs (parseAndCheckTerm rt1)
    t2 <- Env.bindMany bs (parseAndCheckTerm rt2)
    return (Equals name ty_vars bs t1 t2)
  
  defineType :: TypeDef -> Parse m ()
  defineType ((ind_name, ty_vars), raw_cons) = do
    cons <- mapM mkCon raw_cons
    let p_ind = Type.Forall ty_vars (Type.Ind ind_name cons)
    Parser.defineInd ind_name p_ind
    mapM_ (defCon p_ind) [0..nlength raw_cons - 1]
    where
    mkCon :: [String] -> Parse m (String, [Type.ConArg])
    mkCon (con_name:ty_names) = do
      args <- mapM mkArg ty_names
      return (con_name, args)
      where
      mkArg :: String -> Parse m Type.ConArg
      mkArg arg_name
        | arg_name == ind_name = return Type.IndVar
        | otherwise = liftM Type.ConArg (lookupType (arg_name, []))
        
    defCon :: Type.Poly Ind -> Nat -> Parse m ()
    defCon p_ind con_n =
      Parser.defineCon name p_con 
      where
      p_con = fmap (\i -> Constructor i con_n) p_ind
      name = head (raw_cons !! con_n)
  
  defineTerm ((lbl, ty_vars), raw_bs, raw_ret_ty, raw_term) = do
    bs <- mapM parseRawBind raw_bs
    ret_ty <- parseRawType raw_ret_ty
    let full_ty = Type.unflatten (map Type.get bs ++ [ret_ty])
        p_name = Type.Forall ty_vars (Typed (Name lbl) full_ty) 
    Parser.defineName lbl p_name
    term <- parseAndCheckTerm raw_term
    Defs.put (fmap typedObj p_name) bs term

    
lookupType :: InstName -> Parse m Type
lookupType (name, raw_ty_args) = do
  mby_ind <- Parser.lookupInd name
  case mby_ind of
    Nothing -> Err.typeNotFound name
    Just p_ind -> do
      ty_args <- mapM parseRawType raw_ty_args
      return (Type.Base (Type.instantiate ty_args p_ind))
    
parseAndCheckTerm :: RawTerm -> Parse m Term
parseAndCheckTerm = 
  Err.check Type.check . parseRawTerm
  
parseRawType :: RawType -> Parse m Type
parseRawType (TyBase name) =
  lookupType name
parseRawType (TyFun t1 t2) = 
  return Type.Fun `ap` parseRawType t1 `ap` parseRawType t2

parseRawBind :: RawBind -> Parse m Bind
parseRawBind (TBind label raw_ty) = do
  ty <- parseRawType raw_ty
  return (Bind label ty)
  
  
-- This logic is used by assertion parsing and pattern match parsing.
-- It takes the list of strings which will be given as a matched pattern,
-- and the inductive type it is matching on, and returns the constructor
-- index and the new bindings of the pattern variables.
parsePattern :: Inst Ind -> [String] -> Parse m (Nat, [Bind])
parsePattern ind (con_lbl:var_lbls) 
  | null mby_con_i = 
    Err.invalidConstructor (show ind) con_lbl
  | otherwise = 
    return (enum con_i, var_bs)
  where
  cons = Type.unfold ind

  -- Find the type of this particular constructor from looking it up
  -- by name in the description of the inductive type.
  mby_con_i = findIndices ((== con_lbl) . bindLabel) cons
  con_i = head mby_con_i
  this_con = cons !! con_i
  
  -- The bindings for this constructor are the arguments for the type
  con_tys = (init . Type.flatten . Type.get) this_con
  var_bs = zipWith Bind var_lbls con_tys
  

parseRawTerm :: RawTerm -> Parse m Term

parseRawTerm (TUnr rty) = do
  ty <- parseRawType rty
  return (Unr ty)
parseRawTerm (TLam rbs rt) = do
  bs <- mapM parseRawBind rbs
  t <- Env.bindMany bs (parseRawTerm rt)
  return (Term.unflattenLam bs t)

parseRawTerm (TApp (name, raw_ty_args) raw_args) = do
  args <- mapM parseRawTerm raw_args
  ty_args <- mapM parseRawType raw_ty_args
  mby_x <- asks (Map.lookup name . get bindMap)
  case mby_x of
    Just x -> do
      -- Variables take no type arguments
      when (length ty_args > 0) 
        $ Err.invalidTypeArgs name (map show ty_args)
      return (Var x args)
    Nothing -> do
      mby_def <- Parser.lookupTerm name
      case mby_def of
        Just (Parser.DefinedName p_name) -> 
          return (Def (Type.instantiate ty_args p_name) args)
        Just (Parser.DefinedCon p_con) -> 
          return (Con (Type.instantiate ty_args p_con) args)
        Nothing ->
          Err.termNotFound name
          
parseRawTerm (TAssert pat ron_t rin_t) = do
  on_t <- parseRawTerm ron_t
  on_ty@(Type.Base iind) <- Type.getM on_t
  (con_n, var_bs) <- parsePattern iind pat
  in_t <- Env.bindMany var_bs (parseRawTerm rin_t)
  in_ty <- Env.bindMany var_bs (Type.getM in_t)
  let con = fmap (\i -> Constructor i con_n) iind
      assrt = Constraint.make con on_t 
  return (Constraint.apply assrt (Type.specify in_t in_ty))
  
parseRawTerm (TCase rt ralts) = do
  t <- parseRawTerm rt
  ind_ty <- Type.getM t
  unless (Type.isInd ind_ty)
    $ Err.nonInductiveMatch (show ind_ty)
  let ind = Type.inductiveType ind_ty
  alts <- mapM (parseRawAlt ind) ralts
  let alts' = map snd (sortBy (compare `on` fst) alts)
  return (Case t alts')
  where
  parseRawAlt ind (TAlt pat ralt_t) = do
    (con_n, var_bs) <- parsePattern ind pat
    let con = fmap (\i -> Constructor i con_n) ind
    t <- Env.bindMany var_bs (parseRawTerm ralt_t)
    return (con_n, Alt con var_bs t)
    
    
data Token
  = TokenBar
  | TokenName String
  | TokenTypeOf
  | TokenSet
  | TokenLArr
  | TokenRArr
  | TokenDRArr
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
  | TokenUnr
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
  | TokenAssert
  
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
lexer ('-':'>':cs) = TokenRArr : lexer cs
lexer ('<':'-':cs) = TokenLArr : lexer cs
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
      ("unr", '[':rest) -> TokenUnr : lexer rest
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
      ("assert", rest) -> TokenAssert : lexer rest
      (name, rest) -> TokenName name : lexer rest
lexer cs = error $ "Unrecognized symbol " ++ take 1 cs

instance Show Token where
  show TokenBar = "|"
  show (TokenName x) = x
  show TokenTypeOf = ":"
  show TokenRArr = "->"
  show TokenLArr = "<-"
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
  show TokenUnr = "unr["
  show (TokenInj n) = "inj" ++ show n
  show TokenAssert = "assert"
  
  showList = (++) . intercalate " " . map show
}
