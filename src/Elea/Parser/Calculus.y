{
-- | A parser for Elea's raw input calculus: simply typed lambda calculus
-- with anonymous fixpoints, anonymous inductive data types, pattern matching,
-- and explicit absurdity.
module Elea.Parser.Calculus
(
  Env,
  program, term, _type, bindings
)
where

import Elea.Prelude
import Elea.Type hiding ( get )
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Term.Constraint as Constraint
import qualified Elea.Term.Index as Indices
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Ext as Term
import qualified Elea.Type.Ext as Type
import qualified Elea.Monad.Env as Env
import qualified Elea.Foldable as Fold
import qualified Elea.Foldable.WellFormed as WF
import qualified Elea.Transform.Simplify as Simp
import qualified Elea.Transform.Evaluate as Eval
import qualified Elea.Monad.Definitions.Class as Defs      
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Error.Assertion as Assert
import qualified Data.Map as Map

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
  '=<'        { TokenLeq }
  '>='        { TokenGeq }
  '=='        { TokenEqEq }
  '*'         { TokenSet }
  '='         { TokenEq }
  '['         { TokenOS }
  ']'         { TokenCS }
  '('         { TokenOP }
  ')'         { TokenCP }
  '<'         { TokenOA }
  '>'         { TokenCA }
  ','         { TokenComma }
  '_|_'       { TokenUnr }
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
  seq         { TokenSeq }
  'fold['     { TokenFold  }
  assert      { TokenAssert }
  assertBool  { TokenAssBool }
  
%right '->'
  
%%

Type :: { RawType }
  : ParamCall                         { TyBase $1 }
  | '(' Type ')'                      { $2 }
  | '(' Type ',' TypeList ')'         { TyTuple ($2:$4) }
  | Type '->' Type                    { TyFun $1 $3 }
  
Cons :: { [[String]] }
  : Pattern                           { [$1] }
  | Cons '|' Pattern                  { $1 ++ [$3] }
  
TypeDef :: { TypeDef }
  : ind ParamName '=' Cons            { ($2, $4) }
  
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
  : ParamCall                         { TVar $1 }
  | '_|_' Type                        { TUnr $2 }
  | Term ParamCall                    { TApp $1 (TVar $2) }
  | Term '(' Term ')'                 { TApp $1 $3 }
  | '(' Term ')'                      { $2 }
  | Term '(' Term ',' TermList ')'    { TApp $1 (TTuple ($3:$5)) }
  | '(' Term ',' TermList ')'         { TTuple ($2:$4) }
  | fun Bindings '->' Term            { TLam $2 $4 }
  | fix Bindings '->' Term            { TFix $2 $4 }
  | seq Term in Term                  { TSeq $2 $4 }
  | Term '=<' Term                    { TLeq $1 $3 }
  | Term '>=' Term                    { TLeq $3 $1 }
  | Term '==' Term                    { TEq $1 $3 }
  | let name '=' Term in Term         { TLet $2 $4 $6 }
  | 'fold[' Type ']'                  { TFold $2 }
  | match Term with Matches end       { TCase $2 $4 }
  | assert Pattern '<-' Term in Term  { TAssert $2 $4 $6 }
  | assertBool Term in Term           { TAssert [] $2 $4 }
  | if Term then Term else Term       { TCase $2 [ TAlt ["True"] $4
                                                 , TAlt ["False"] $6] }
                                                 
TermDef :: { TermDef }
  : let ParamName '=' Term            { ($2, $4) }
  
ParamCall :: { ParamCall }
  : name                              { ($1, []) }
  | name '<' TypeList '>'             { ($1, $3) }
  
ParamName :: { ParamName }
  : name                              { ($1, []) }
  | name '<' NameList '>'             { ($1, $3) } 
  
PropDef :: { PropDef }
  : prop ParamName Bindings '->' Term  { ($2, $3, $5) }
  | prop ParamName '->' Term           { ($2, [], $4) }
  | prop '<' NameList '>' Bindings '->' Term 
                                       { (("", $3), $5, $7) }
  | prop Bindings '->' Term            { (("", []), $2, $4) }

Program :: { RawProgram }
  : {- empty -}                       { RawProgram [] [] [] }
  | Program TypeDef                   { modify programTypes (++ [$2]) $1 }
  | Program TermDef                   { modify programTerms (++ [$2]) $1 }
  | Program PropDef                   { modify programProps (++ [$2]) $1 }

{


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
type PropDef = (ParamName, [RawBind], RawTerm)

data RawProgram 
  = RawProgram  { _programTypes :: [TypeDef]
                , _programTerms :: [TermDef]
                , _programProps :: [PropDef] }

data RawType 
  = TyBase ParamCall
  | TyFun RawType RawType
  | TyTuple [RawType]

data RawBind 
  = TBind { rawLabel :: String
          , rawType :: RawType }

data RawTerm
  = TVar ParamCall
  | TApp RawTerm RawTerm
  | TFix [RawBind] RawTerm
  | TLam [RawBind] RawTerm
  | TSeq RawTerm RawTerm
  | TCon Nat RawType
  | TUnr RawType
  | TCase RawTerm [RawAlt]
  | TLet String RawTerm RawTerm
  | TLeq RawTerm RawTerm
  | TEq RawTerm RawTerm
  | TFold RawType
  | TTuple [RawTerm]
  | TAssert [String] RawTerm RawTerm
  
data RawAlt
  = TAlt [String] RawTerm
  
data Scope 
  = Scope { _bindMap :: Map String Term
          , _bindStack :: [Bind]
          , _typeArgs :: Map String Type }
          
      
-- mkLabels wasn't working with Happy, so here I am deriving my lenses
-- myself like a scrub
programTypes :: RawProgram :-> [TypeDef]
programTypes = lens _programTypes 
  (\f x -> x { _programTypes = f (_programTypes x) }) 
  
programTerms :: RawProgram :-> [TermDef]
programTerms = lens _programTerms
  (\f x -> x { _programTerms = f (_programTerms x) })  
  
programProps :: RawProgram :-> [PropDef]
programProps = lens _programProps
  (\f x -> x { _programProps = f (_programProps x) }) 
  
bindMap :: Scope :-> Map String Term
bindMap = lens _bindMap
  (\f x -> x { _bindMap = f (_bindMap x) }) 
  
bindStack :: Scope :-> [Bind]
bindStack = lens _bindStack
  (\f x -> x { _bindStack = f (_bindStack x) }) 
  
typeArgs :: Scope :-> Map String Type
typeArgs = lens _typeArgs
  (\f x -> x { _typeArgs = f (_typeArgs x) }) 
  

withEmptyScope :: ReaderT Scope m a -> m a
withEmptyScope = flip runReaderT (Scope mempty mempty mempty)

instance Monad m => Env.Write (ReaderT Scope m) where
  bindAt at b = 
      local
    $ modify bindMap (addToMap . map (Indices.liftAt at))
    . modify bindStack addToStack
    where
    addToStack = insertAt (enum at) b
    addToMap = Map.insert (get bindLabel b) (Var at b)
    
  matched _ = id
  forgetMatches _ = id
  
instance Monad m => Env.Read (ReaderT Scope m) where
  bindings = asks (get bindStack)
  
type ParserMonad m a = 
  Env m => ReaderT Scope m a
  
type Env m = (Err.Throws Err.Stack m, Defs.Has m, Tag.Gen m)

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
  
term :: (Env m, Env.Read m) => String -> m Term
term str = do
  bs <- Env.bindings
  withEmptyScope
    . liftM Eval.apply
    . Env.bindMany (reverse bs)
    . parseAndCheckTerm 
    . happyTerm 
    . lexer
    $ str
  
_type :: Env m => String -> m Type
_type = id
  . withEmptyScope
  . parseRawType
  . happyType 
  . lexer
  
bindings :: Env m => String -> m [Bind]
bindings = id
  . withEmptyScope
  . mapM parseRawBind
  . happyBindings
  . lexer
  
program :: forall m . Env m 
  => String -> m [Polymorphic Prop]
program text = 
  withEmptyScope $ do
    mapM_ defineType types
    mapM_ defineTerm terms
    mapM parseProp props
  where
  RawProgram types terms props = happyProgram (lexer text)
    
  parseProp :: PropDef -> ParserMonad m (Polymorphic Prop)
  parseProp ((name, ty_args), rbs, rt) = 
    localTypeArgs ty_args $ do
      bs <- mapM parseRawBind rbs
      t <- Env.bindMany bs (parseAndCheckTerm rt)
      ty <- Type.getM t
      let t' | Type.returnType ty == Type.bool = Term.Leq t (Term.true)
             | otherwise = t
      return (Prop name (unflattenLam bs t'))
  
  defineType :: TypeDef -> ParserMonad m ()
  defineType ((ind_name, ty_args), raw_cons) = do
    ind_ty <- localTypeArgs ty_args $ do
      cons <- mapM mkCon raw_cons
      return (Type.Ind ind_name cons)
    Defs.defineType ind_name ind_ty
    mapM_ (defCon ind_ty) (range raw_cons)
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
        
    defCon :: Polymorphic Ind -> Nat -> ParserMonad m ()
    defCon poly_ind n =
      Defs.defineTerm name poly_con 
      where
      poly_con = fmap untaggedCon poly_ind
      name = head (raw_cons !! n)   
      untaggedCon ind = Con (Tag.with Tag.null (Constructor ind (enum n)))
  
  defineTerm ((name, ty_args), raw_term) = do
    p_term <- localTypeArgs ty_args (parseAndCheckTerm raw_term)
    Defs.defineTerm name 
      $ fmap (Simp.apply . nameFix) p_term
    where
    nameFix (Fix inf b t) = 
      Fix (set fixName (Just (printf "$%s" name)) inf) b t
    nameFix other_t = other_t
    
lookupTerm :: ParamCall -> ParserMonad m Term
lookupTerm (name, raw_ty_args) = do
  mby_bind <- asks (Map.lookup name . get bindMap)
  if length raw_ty_args == 0 && isJust mby_bind
  then return (fromJust mby_bind)
  else do
    ty_args <- mapM parseRawType raw_ty_args
    mby_term <- Defs.lookupTerm name ty_args
    case mby_term of
      Nothing -> Err.throwf "undefined term %s" name
      Just (Con null_tcon) -> do
        tcon <- Tag.tag (Tag.untag null_tcon)
        return (Con tcon)
      Just other_t -> return other_t
    
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
    else Err.throwf "undefined inductive type %s" name
    
parseAndCheckTerm :: RawTerm -> ParserMonad m Term
parseAndCheckTerm rt = do
  t <- parseRawTerm rt
  Assert.toError (WF.assert t)
  return t
  
parseRawType :: RawType -> ParserMonad m Type
parseRawType (TyBase name) =
  lookupType name
parseRawType (TyFun t1 t2) = 
  return Type.Fun `ap` parseRawType t1 `ap` parseRawType t2
parseRawType (TyTuple rtys) = do
  tys <- mapM parseRawType rtys
  return (Type.Base (Type.tuple tys))

parseRawBind :: RawBind -> ParserMonad m Bind
parseRawBind (TBind label raw_ty) = do
  ty <- parseRawType raw_ty
  return (Bind label ty)
  
  
-- This logic is used by assertion parsing and pattern match parsing.
-- It takes the list of strings which will be given as a matched pattern,
-- and the inductive type it is matching on, and returns the constructor
-- index and the new bindings of the pattern variables.
parsePattern :: Ind -> [String] -> ParserMonad m (Nat, [Bind])
parsePattern ind (con_lbl : var_lbls) 
  | null mby_con_i = id
    . Err.augmentf "parsing pattern for \"\"" ind
    $ Err.throwf "invalid constructor \"%s\"" con_lbl
  | otherwise = 
    return (enum con_i, var_bs)
  where
  cons = Type.unfold ind

  -- Find the type of this particular constructor from looking it up
  -- by name in the description of the inductive type.
  mby_con_i = findIndices ((== con_lbl) . get bindLabel) cons
  con_i = head mby_con_i
  this_con = cons !! con_i
  
  -- The bindings for this constructor are the arguments for the type
  con_tys = (init . Type.flatten . get bindType) this_con
  var_bs = zipWith Bind var_lbls con_tys
  

parseRawTerm :: RawTerm -> ParserMonad m Term
parseRawTerm (TTuple rts) = do
  ts <- mapM parseRawTerm rts
  Term.tuple ts
parseRawTerm (TFold raw_ty) = do
  ty <- parseRawType raw_ty
  case ty of
    Fun (Base ind) res_ty ->
      return (buildFold ind res_ty)
    _ -> error 
      $ "Fold syntax given invalid type [" ++ show ty ++ "]."
      ++ "This must be a function from an inductive type."
parseRawTerm (TLeq rt1 rt2) = do
  t1 <- parseRawTerm rt1
  t2 <- parseRawTerm rt2
  return (Term.Leq t1 t2)
parseRawTerm (TEq rt1 rt2) = do
  t1 <- parseRawTerm rt1
  t2 <- parseRawTerm rt2 
  return (conj [Term.Leq t1 t2, Term.Leq t2 t1])
parseRawTerm (TSeq rt1 rt2) = do
  t1 <- parseRawTerm rt1
  t2 <- parseRawTerm rt2
  return (Term.Seq t1 t2)
parseRawTerm (TVar var) = 
  lookupTerm var
parseRawTerm (TUnr rty) = do
  ty <- parseRawType rty
  return (Term.Bot ty)
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
parseRawTerm (TAssert pat ron_t rin_t) = do
  on_t <- parseRawTerm ron_t
  on_ty@(Type.Base ind) <- Type.getM on_t
  (con_n, var_bs) <- parsePattern ind pat'
  tcon <- Tag.tag (Constructor ind con_n)
  in_t <- Env.bindMany var_bs (parseRawTerm rin_t)
  in_ty <- Env.bindMany var_bs (Type.getM in_t)
  let ct = Term.matchFromConstructor tcon on_t
  return (Constraint.apply in_ty ct in_t)
  where
  pat' | pat == [] = ["True"]
       | otherwise = pat
  
parseRawTerm (TCase rt ralts) = do
  t <- parseRawTerm rt
  ind_ty <- Type.getM t
  alts <- mapM (parseRawAlt ind_ty) ralts
  let alts' = map snd (sortBy (compare `on` fst) alts)
  return (Case t alts')
  where
  parseRawAlt ind_ty (TAlt pat ralt_t)
    | not (Type.isInd ind_ty) =
      Err.throwf "pattern matching over non inductive type \"%s\"" ind_ty
    | otherwise = do
      (con_n, var_bs) <- parsePattern ind pat
      t <- Env.bindMany var_bs (parseRawTerm ralt_t)
      con <-  Tag.tag (Constructor ind con_n)
      return (con_n, Alt con var_bs t)
    where 
    Type.Base ind = ind_ty
      
data Token
  = TokenBar
  | TokenName String
  | TokenTypeOf
  | TokenSet
  | TokenLArr
  | TokenRArr
  | TokenDRArr
  | TokenLeq
  | TokenGeq
  | TokenSeq
  | TokenEq
  | TokenOS
  | TokenCS
  | TokenOP
  | TokenCP
  | TokenOA
  | TokenCA
  | TokenComma
  | TokenUnr
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
  | TokenAssert
  | TokenAssBool
  
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
lexer ('/':'/':cs) = lexer (dropWhile (/= '\n') cs)
lexer (' ':cs) = lexer cs
lexer ('\n':cs) = lexer cs
lexer ('-':'>':cs) = TokenRArr : lexer cs
lexer ('<':'-':cs) = TokenLArr : lexer cs
lexer ('>':'=':cs) = TokenGeq : lexer cs
lexer ('=':'=':cs) = TokenEqEq : lexer cs
lexer ('=':'<':cs) = TokenLeq : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('_':'|':'_':cs) = TokenUnr : lexer cs
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
      ("seq", rest) -> TokenSeq : lexer rest
      ("end", rest) -> TokenEnd : lexer rest
      ("prop", rest) -> TokenProp : lexer rest
      ("forall", rest) -> TokenAll : lexer rest
      ("fold", '[':rest) -> TokenFold : lexer rest
      ("assertBool", rest) -> TokenAssBool : lexer rest
      ("assert", rest) -> TokenAssert : lexer rest
      (name, rest) -> TokenName name : lexer rest
lexer cs = error $ "Unrecognized symbol " ++ take 1 cs

instance Show Token where
  show TokenBar = "|"
  show (TokenName x) = x
  show TokenTypeOf = ":"
  show TokenRArr = "->"
  show TokenLArr = "<-"
  show TokenLeq = "=<"
  show TokenEq = "="
  show TokenUnr = "_|_"
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
  show TokenEqEq = "=="
  show TokenGeq = ">="
  show (TokenInj n) = "inj" ++ show n
  show TokenAssert = "assert"
  show TokenSeq = "seq"
  
  showList = (++) . intercalate " " . map show
}
