{-# OPTIONS_GHC -w #-}
-- | A parser for Elea's raw input calculus: simply typed lambda calculus
-- with anonymous fixpoints, anonymous inductive data types, pattern matching,
-- and explicit absurdity.
module Elea.Parser.Calculus
(
  program, term, _type, bindings
)
where

import Elea.Prelude
import Elea.Type hiding ( get )
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Term.Index as Indices
import qualified Elea.Term.Ext as Term
import qualified Elea.Type.Ext as Type
import qualified Elea.Constraint as Constraint
import qualified Elea.Monad.Env as Env
import qualified Elea.Foldable as Fold
import qualified Elea.Transform.Simplify as Simp
import qualified Elea.Transform.Evaluate as Eval
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
  = TBind { _rawLabel :: String
          , _rawType :: RawType }

data RawTerm
  = TVar ParamCall
  | TApp RawTerm RawTerm
  | TFix [RawBind] RawTerm
  | TLam [RawBind] RawTerm
  | TCon Nat RawType
  | TUnr RawType
  | TCase RawTerm [RawAlt]
  | TLet String RawTerm RawTerm
  | TEql RawTerm RawTerm
  | TFold RawType
  | TTuple [RawTerm]
  | TAssert [String] RawTerm RawTerm
  
data RawAlt
  = TAlt [String] RawTerm
  
data Scope 
  = Scope { _bindMap :: Map String Term
          , _bindStack :: [Bind]
          , _typeArgs :: Map String Type }
  
mkLabels [''Scope, ''RawBind, ''RawProgram]

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn7 (RawType)
	| HappyAbsSyn8 ([[String]])
	| HappyAbsSyn9 (TypeDef)
	| HappyAbsSyn10 ([String])
	| HappyAbsSyn12 ([RawTerm])
	| HappyAbsSyn13 ([RawType])
	| HappyAbsSyn15 ([RawAlt])
	| HappyAbsSyn16 (RawAlt)
	| HappyAbsSyn17 ([RawBind])
	| HappyAbsSyn19 (RawTerm)
	| HappyAbsSyn20 (TermDef)
	| HappyAbsSyn21 (ParamCall)
	| HappyAbsSyn22 (ParamName)
	| HappyAbsSyn23 (PropDef)
	| HappyAbsSyn24 (RawProgram)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (24) = happyGoto action_23
action_0 _ = happyReduce_49

action_1 (25) = happyShift action_6
action_1 (35) = happyShift action_14
action_1 (40) = happyShift action_15
action_1 (41) = happyShift action_16
action_1 (43) = happyShift action_17
action_1 (44) = happyShift action_18
action_1 (45) = happyShift action_19
action_1 (53) = happyShift action_20
action_1 (56) = happyShift action_21
action_1 (58) = happyShift action_22
action_1 (19) = happyGoto action_12
action_1 (21) = happyGoto action_13
action_1 _ = happyFail

action_2 (25) = happyShift action_6
action_2 (35) = happyShift action_11
action_2 (7) = happyGoto action_10
action_2 (21) = happyGoto action_5
action_2 _ = happyFail

action_3 (35) = happyShift action_9
action_3 (17) = happyGoto action_7
action_3 (18) = happyGoto action_8
action_3 _ = happyFail

action_4 (25) = happyShift action_6
action_4 (21) = happyGoto action_5
action_4 _ = happyFail

action_5 _ = happyReduce_4

action_6 (37) = happyShift action_49
action_6 _ = happyReduce_43

action_7 _ = happyReduce_25

action_8 (35) = happyShift action_9
action_8 (59) = happyAccept
action_8 (17) = happyGoto action_48
action_8 _ = happyFail

action_9 (25) = happyShift action_32
action_9 (35) = happyShift action_33
action_9 (10) = happyGoto action_30
action_9 (14) = happyGoto action_47
action_9 _ = happyFail

action_10 (28) = happyShift action_46
action_10 (59) = happyAccept
action_10 _ = happyFail

action_11 (25) = happyShift action_6
action_11 (35) = happyShift action_11
action_11 (7) = happyGoto action_45
action_11 (21) = happyGoto action_5
action_11 _ = happyFail

action_12 (25) = happyShift action_6
action_12 (32) = happyShift action_43
action_12 (35) = happyShift action_44
action_12 (59) = happyAccept
action_12 (21) = happyGoto action_42
action_12 _ = happyFail

action_13 _ = happyReduce_27

action_14 (25) = happyShift action_6
action_14 (35) = happyShift action_14
action_14 (40) = happyShift action_15
action_14 (41) = happyShift action_16
action_14 (43) = happyShift action_17
action_14 (44) = happyShift action_18
action_14 (45) = happyShift action_19
action_14 (53) = happyShift action_20
action_14 (56) = happyShift action_21
action_14 (58) = happyShift action_22
action_14 (19) = happyGoto action_41
action_14 (21) = happyGoto action_13
action_14 _ = happyFail

action_15 (25) = happyShift action_6
action_15 (35) = happyShift action_11
action_15 (7) = happyGoto action_40
action_15 (21) = happyGoto action_5
action_15 _ = happyFail

action_16 (25) = happyShift action_6
action_16 (35) = happyShift action_14
action_16 (40) = happyShift action_15
action_16 (41) = happyShift action_16
action_16 (43) = happyShift action_17
action_16 (44) = happyShift action_18
action_16 (45) = happyShift action_19
action_16 (53) = happyShift action_20
action_16 (56) = happyShift action_21
action_16 (58) = happyShift action_22
action_16 (19) = happyGoto action_39
action_16 (21) = happyGoto action_13
action_16 _ = happyFail

action_17 (35) = happyShift action_9
action_17 (17) = happyGoto action_7
action_17 (18) = happyGoto action_38
action_17 _ = happyFail

action_18 (35) = happyShift action_9
action_18 (17) = happyGoto action_7
action_18 (18) = happyGoto action_37
action_18 _ = happyFail

action_19 (25) = happyShift action_36
action_19 _ = happyFail

action_20 (25) = happyShift action_6
action_20 (35) = happyShift action_14
action_20 (40) = happyShift action_15
action_20 (41) = happyShift action_16
action_20 (43) = happyShift action_17
action_20 (44) = happyShift action_18
action_20 (45) = happyShift action_19
action_20 (53) = happyShift action_20
action_20 (56) = happyShift action_21
action_20 (58) = happyShift action_22
action_20 (19) = happyGoto action_35
action_20 (21) = happyGoto action_13
action_20 _ = happyFail

action_21 (25) = happyShift action_6
action_21 (35) = happyShift action_11
action_21 (7) = happyGoto action_34
action_21 (21) = happyGoto action_5
action_21 _ = happyFail

action_22 (25) = happyShift action_32
action_22 (35) = happyShift action_33
action_22 (10) = happyGoto action_30
action_22 (14) = happyGoto action_31
action_22 _ = happyFail

action_23 (45) = happyShift action_27
action_23 (50) = happyShift action_28
action_23 (51) = happyShift action_29
action_23 (59) = happyAccept
action_23 (9) = happyGoto action_24
action_23 (20) = happyGoto action_25
action_23 (23) = happyGoto action_26
action_23 _ = happyFail

action_24 _ = happyReduce_50

action_25 _ = happyReduce_51

action_26 _ = happyReduce_52

action_27 (25) = happyShift action_70
action_27 (22) = happyGoto action_72
action_27 _ = happyFail

action_28 (25) = happyShift action_70
action_28 (22) = happyGoto action_71
action_28 _ = happyFail

action_29 (25) = happyShift action_70
action_29 (22) = happyGoto action_69
action_29 _ = happyFail

action_30 (25) = happyShift action_68
action_30 _ = happyReduce_19

action_31 (29) = happyShift action_67
action_31 _ = happyFail

action_32 _ = happyReduce_11

action_33 (25) = happyShift action_66
action_33 _ = happyFail

action_34 (28) = happyShift action_46
action_34 (34) = happyShift action_65
action_34 _ = happyFail

action_35 (25) = happyShift action_6
action_35 (32) = happyShift action_43
action_35 (35) = happyShift action_44
action_35 (54) = happyShift action_64
action_35 (21) = happyGoto action_42
action_35 _ = happyFail

action_36 (32) = happyShift action_63
action_36 _ = happyFail

action_37 (28) = happyShift action_62
action_37 (35) = happyShift action_9
action_37 (17) = happyGoto action_48
action_37 _ = happyFail

action_38 (28) = happyShift action_61
action_38 (35) = happyShift action_9
action_38 (17) = happyGoto action_48
action_38 _ = happyFail

action_39 (25) = happyShift action_6
action_39 (32) = happyShift action_43
action_39 (35) = happyShift action_44
action_39 (42) = happyShift action_60
action_39 (21) = happyGoto action_42
action_39 _ = happyFail

action_40 (28) = happyShift action_46
action_40 _ = happyReduce_28

action_41 (25) = happyShift action_6
action_41 (32) = happyShift action_43
action_41 (35) = happyShift action_44
action_41 (36) = happyShift action_58
action_41 (39) = happyShift action_59
action_41 (21) = happyGoto action_42
action_41 _ = happyFail

action_42 _ = happyReduce_29

action_43 (25) = happyShift action_6
action_43 (35) = happyShift action_14
action_43 (40) = happyShift action_15
action_43 (41) = happyShift action_16
action_43 (43) = happyShift action_17
action_43 (44) = happyShift action_18
action_43 (45) = happyShift action_19
action_43 (53) = happyShift action_20
action_43 (56) = happyShift action_21
action_43 (58) = happyShift action_22
action_43 (19) = happyGoto action_57
action_43 (21) = happyGoto action_13
action_43 _ = happyFail

action_44 (25) = happyShift action_6
action_44 (35) = happyShift action_14
action_44 (40) = happyShift action_15
action_44 (41) = happyShift action_16
action_44 (43) = happyShift action_17
action_44 (44) = happyShift action_18
action_44 (45) = happyShift action_19
action_44 (53) = happyShift action_20
action_44 (56) = happyShift action_21
action_44 (58) = happyShift action_22
action_44 (19) = happyGoto action_56
action_44 (21) = happyGoto action_13
action_44 _ = happyFail

action_45 (28) = happyShift action_46
action_45 (36) = happyShift action_54
action_45 (39) = happyShift action_55
action_45 _ = happyFail

action_46 (25) = happyShift action_6
action_46 (35) = happyShift action_11
action_46 (7) = happyGoto action_53
action_46 (21) = happyGoto action_5
action_46 _ = happyFail

action_47 (27) = happyShift action_52
action_47 _ = happyFail

action_48 _ = happyReduce_26

action_49 (25) = happyShift action_6
action_49 (35) = happyShift action_11
action_49 (7) = happyGoto action_50
action_49 (13) = happyGoto action_51
action_49 (21) = happyGoto action_5
action_49 _ = happyFail

action_50 (28) = happyShift action_46
action_50 _ = happyReduce_17

action_51 (38) = happyShift action_91
action_51 (39) = happyShift action_92
action_51 _ = happyFail

action_52 (25) = happyShift action_6
action_52 (35) = happyShift action_11
action_52 (7) = happyGoto action_90
action_52 (21) = happyGoto action_5
action_52 _ = happyFail

action_53 (28) = happyShift action_46
action_53 _ = happyReduce_7

action_54 _ = happyReduce_5

action_55 (25) = happyShift action_6
action_55 (35) = happyShift action_11
action_55 (7) = happyGoto action_50
action_55 (13) = happyGoto action_89
action_55 (21) = happyGoto action_5
action_55 _ = happyFail

action_56 (25) = happyShift action_6
action_56 (32) = happyShift action_43
action_56 (35) = happyShift action_44
action_56 (36) = happyShift action_87
action_56 (39) = happyShift action_88
action_56 (21) = happyGoto action_42
action_56 _ = happyFail

action_57 (25) = happyShift action_6
action_57 (32) = happyShift action_43
action_57 (35) = happyShift action_44
action_57 (21) = happyGoto action_42
action_57 _ = happyReduce_36

action_58 _ = happyReduce_31

action_59 (25) = happyShift action_6
action_59 (35) = happyShift action_14
action_59 (40) = happyShift action_15
action_59 (41) = happyShift action_16
action_59 (43) = happyShift action_17
action_59 (44) = happyShift action_18
action_59 (45) = happyShift action_19
action_59 (53) = happyShift action_20
action_59 (56) = happyShift action_21
action_59 (58) = happyShift action_22
action_59 (12) = happyGoto action_85
action_59 (19) = happyGoto action_86
action_59 (21) = happyGoto action_13
action_59 _ = happyFail

action_60 (26) = happyShift action_84
action_60 (15) = happyGoto action_83
action_60 _ = happyFail

action_61 (25) = happyShift action_6
action_61 (35) = happyShift action_14
action_61 (40) = happyShift action_15
action_61 (41) = happyShift action_16
action_61 (43) = happyShift action_17
action_61 (44) = happyShift action_18
action_61 (45) = happyShift action_19
action_61 (53) = happyShift action_20
action_61 (56) = happyShift action_21
action_61 (58) = happyShift action_22
action_61 (19) = happyGoto action_82
action_61 (21) = happyGoto action_13
action_61 _ = happyFail

action_62 (25) = happyShift action_6
action_62 (35) = happyShift action_14
action_62 (40) = happyShift action_15
action_62 (41) = happyShift action_16
action_62 (43) = happyShift action_17
action_62 (44) = happyShift action_18
action_62 (45) = happyShift action_19
action_62 (53) = happyShift action_20
action_62 (56) = happyShift action_21
action_62 (58) = happyShift action_22
action_62 (19) = happyGoto action_81
action_62 (21) = happyGoto action_13
action_62 _ = happyFail

action_63 (25) = happyShift action_6
action_63 (35) = happyShift action_14
action_63 (40) = happyShift action_15
action_63 (41) = happyShift action_16
action_63 (43) = happyShift action_17
action_63 (44) = happyShift action_18
action_63 (45) = happyShift action_19
action_63 (53) = happyShift action_20
action_63 (56) = happyShift action_21
action_63 (58) = happyShift action_22
action_63 (19) = happyGoto action_80
action_63 (21) = happyGoto action_13
action_63 _ = happyFail

action_64 (25) = happyShift action_6
action_64 (35) = happyShift action_14
action_64 (40) = happyShift action_15
action_64 (41) = happyShift action_16
action_64 (43) = happyShift action_17
action_64 (44) = happyShift action_18
action_64 (45) = happyShift action_19
action_64 (53) = happyShift action_20
action_64 (56) = happyShift action_21
action_64 (58) = happyShift action_22
action_64 (19) = happyGoto action_79
action_64 (21) = happyGoto action_13
action_64 _ = happyFail

action_65 _ = happyReduce_38

action_66 (39) = happyShift action_78
action_66 _ = happyFail

action_67 (25) = happyShift action_6
action_67 (35) = happyShift action_14
action_67 (40) = happyShift action_15
action_67 (41) = happyShift action_16
action_67 (43) = happyShift action_17
action_67 (44) = happyShift action_18
action_67 (45) = happyShift action_19
action_67 (53) = happyShift action_20
action_67 (56) = happyShift action_21
action_67 (58) = happyShift action_22
action_67 (19) = happyGoto action_77
action_67 (21) = happyGoto action_13
action_67 _ = happyFail

action_68 _ = happyReduce_12

action_69 (27) = happyShift action_76
action_69 _ = happyFail

action_70 (37) = happyShift action_75
action_70 _ = happyReduce_45

action_71 (32) = happyShift action_74
action_71 _ = happyFail

action_72 (32) = happyShift action_73
action_72 _ = happyFail

action_73 (25) = happyShift action_6
action_73 (35) = happyShift action_14
action_73 (40) = happyShift action_15
action_73 (41) = happyShift action_16
action_73 (43) = happyShift action_17
action_73 (44) = happyShift action_18
action_73 (45) = happyShift action_19
action_73 (53) = happyShift action_20
action_73 (56) = happyShift action_21
action_73 (58) = happyShift action_22
action_73 (19) = happyGoto action_113
action_73 (21) = happyGoto action_13
action_73 _ = happyFail

action_74 (25) = happyShift action_32
action_74 (35) = happyShift action_33
action_74 (8) = happyGoto action_111
action_74 (10) = happyGoto action_30
action_74 (14) = happyGoto action_112
action_74 _ = happyFail

action_75 (25) = happyShift action_106
action_75 (11) = happyGoto action_110
action_75 _ = happyFail

action_76 (25) = happyShift action_6
action_76 (35) = happyShift action_14
action_76 (40) = happyShift action_15
action_76 (41) = happyShift action_16
action_76 (43) = happyShift action_17
action_76 (44) = happyShift action_18
action_76 (45) = happyShift action_19
action_76 (52) = happyShift action_109
action_76 (53) = happyShift action_20
action_76 (56) = happyShift action_21
action_76 (58) = happyShift action_22
action_76 (19) = happyGoto action_108
action_76 (21) = happyGoto action_13
action_76 _ = happyFail

action_77 (25) = happyShift action_6
action_77 (32) = happyShift action_43
action_77 (35) = happyShift action_44
action_77 (46) = happyShift action_107
action_77 (21) = happyGoto action_42
action_77 _ = happyFail

action_78 (25) = happyShift action_106
action_78 (11) = happyGoto action_105
action_78 _ = happyFail

action_79 (25) = happyShift action_6
action_79 (32) = happyShift action_43
action_79 (35) = happyShift action_44
action_79 (55) = happyShift action_104
action_79 (21) = happyGoto action_42
action_79 _ = happyFail

action_80 (25) = happyShift action_6
action_80 (32) = happyShift action_43
action_80 (35) = happyShift action_44
action_80 (46) = happyShift action_103
action_80 (21) = happyGoto action_42
action_80 _ = happyFail

action_81 (25) = happyShift action_6
action_81 (32) = happyShift action_43
action_81 (35) = happyShift action_44
action_81 (21) = happyGoto action_42
action_81 _ = happyReduce_35

action_82 (25) = happyShift action_6
action_82 (32) = happyShift action_43
action_82 (35) = happyShift action_44
action_82 (21) = happyGoto action_42
action_82 _ = happyReduce_34

action_83 (26) = happyShift action_101
action_83 (48) = happyShift action_102
action_83 _ = happyFail

action_84 (25) = happyShift action_32
action_84 (35) = happyShift action_33
action_84 (10) = happyGoto action_30
action_84 (14) = happyGoto action_99
action_84 (16) = happyGoto action_100
action_84 _ = happyFail

action_85 (36) = happyShift action_97
action_85 (39) = happyShift action_98
action_85 _ = happyFail

action_86 (25) = happyShift action_6
action_86 (32) = happyShift action_43
action_86 (35) = happyShift action_44
action_86 (21) = happyGoto action_42
action_86 _ = happyReduce_15

action_87 _ = happyReduce_30

action_88 (25) = happyShift action_6
action_88 (35) = happyShift action_14
action_88 (40) = happyShift action_15
action_88 (41) = happyShift action_16
action_88 (43) = happyShift action_17
action_88 (44) = happyShift action_18
action_88 (45) = happyShift action_19
action_88 (53) = happyShift action_20
action_88 (56) = happyShift action_21
action_88 (58) = happyShift action_22
action_88 (12) = happyGoto action_96
action_88 (19) = happyGoto action_86
action_88 (21) = happyGoto action_13
action_88 _ = happyFail

action_89 (36) = happyShift action_95
action_89 (39) = happyShift action_92
action_89 _ = happyFail

action_90 (28) = happyShift action_46
action_90 (36) = happyShift action_94
action_90 _ = happyFail

action_91 _ = happyReduce_44

action_92 (25) = happyShift action_6
action_92 (35) = happyShift action_11
action_92 (7) = happyGoto action_93
action_92 (21) = happyGoto action_5
action_92 _ = happyFail

action_93 (28) = happyShift action_46
action_93 _ = happyReduce_18

action_94 _ = happyReduce_24

action_95 _ = happyReduce_6

action_96 (36) = happyShift action_125
action_96 (39) = happyShift action_98
action_96 _ = happyFail

action_97 _ = happyReduce_33

action_98 (25) = happyShift action_6
action_98 (35) = happyShift action_14
action_98 (40) = happyShift action_15
action_98 (41) = happyShift action_16
action_98 (43) = happyShift action_17
action_98 (44) = happyShift action_18
action_98 (45) = happyShift action_19
action_98 (53) = happyShift action_20
action_98 (56) = happyShift action_21
action_98 (58) = happyShift action_22
action_98 (19) = happyGoto action_124
action_98 (21) = happyGoto action_13
action_98 _ = happyFail

action_99 (28) = happyShift action_123
action_99 _ = happyFail

action_100 _ = happyReduce_21

action_101 (25) = happyShift action_32
action_101 (35) = happyShift action_33
action_101 (10) = happyGoto action_30
action_101 (14) = happyGoto action_99
action_101 (16) = happyGoto action_122
action_101 _ = happyFail

action_102 _ = happyReduce_39

action_103 (25) = happyShift action_6
action_103 (35) = happyShift action_14
action_103 (40) = happyShift action_15
action_103 (41) = happyShift action_16
action_103 (43) = happyShift action_17
action_103 (44) = happyShift action_18
action_103 (45) = happyShift action_19
action_103 (53) = happyShift action_20
action_103 (56) = happyShift action_21
action_103 (58) = happyShift action_22
action_103 (19) = happyGoto action_121
action_103 (21) = happyGoto action_13
action_103 _ = happyFail

action_104 (25) = happyShift action_6
action_104 (35) = happyShift action_14
action_104 (40) = happyShift action_15
action_104 (41) = happyShift action_16
action_104 (43) = happyShift action_17
action_104 (44) = happyShift action_18
action_104 (45) = happyShift action_19
action_104 (53) = happyShift action_20
action_104 (56) = happyShift action_21
action_104 (58) = happyShift action_22
action_104 (19) = happyGoto action_120
action_104 (21) = happyGoto action_13
action_104 _ = happyFail

action_105 (36) = happyShift action_119
action_105 (39) = happyShift action_116
action_105 _ = happyFail

action_106 _ = happyReduce_13

action_107 (25) = happyShift action_6
action_107 (35) = happyShift action_14
action_107 (40) = happyShift action_15
action_107 (41) = happyShift action_16
action_107 (43) = happyShift action_17
action_107 (44) = happyShift action_18
action_107 (45) = happyShift action_19
action_107 (53) = happyShift action_20
action_107 (56) = happyShift action_21
action_107 (58) = happyShift action_22
action_107 (19) = happyGoto action_118
action_107 (21) = happyGoto action_13
action_107 _ = happyFail

action_108 (25) = happyShift action_6
action_108 (32) = happyShift action_43
action_108 (35) = happyShift action_44
action_108 (21) = happyGoto action_42
action_108 _ = happyReduce_48

action_109 (35) = happyShift action_9
action_109 (17) = happyGoto action_7
action_109 (18) = happyGoto action_117
action_109 _ = happyFail

action_110 (38) = happyShift action_115
action_110 (39) = happyShift action_116
action_110 _ = happyFail

action_111 (26) = happyShift action_114
action_111 _ = happyReduce_10

action_112 _ = happyReduce_8

action_113 (25) = happyShift action_6
action_113 (32) = happyShift action_43
action_113 (35) = happyShift action_44
action_113 (21) = happyGoto action_42
action_113 _ = happyReduce_42

action_114 (25) = happyShift action_32
action_114 (35) = happyShift action_33
action_114 (10) = happyGoto action_30
action_114 (14) = happyGoto action_129
action_114 _ = happyFail

action_115 _ = happyReduce_46

action_116 (25) = happyShift action_128
action_116 _ = happyFail

action_117 (28) = happyShift action_127
action_117 (35) = happyShift action_9
action_117 (17) = happyGoto action_48
action_117 _ = happyFail

action_118 (25) = happyShift action_6
action_118 (32) = happyShift action_43
action_118 (35) = happyShift action_44
action_118 (21) = happyGoto action_42
action_118 _ = happyReduce_40

action_119 _ = happyReduce_20

action_120 (25) = happyShift action_6
action_120 (32) = happyShift action_43
action_120 (35) = happyShift action_44
action_120 (21) = happyGoto action_42
action_120 _ = happyReduce_41

action_121 (25) = happyShift action_6
action_121 (32) = happyShift action_43
action_121 (35) = happyShift action_44
action_121 (21) = happyGoto action_42
action_121 _ = happyReduce_37

action_122 _ = happyReduce_22

action_123 (25) = happyShift action_6
action_123 (35) = happyShift action_14
action_123 (40) = happyShift action_15
action_123 (41) = happyShift action_16
action_123 (43) = happyShift action_17
action_123 (44) = happyShift action_18
action_123 (45) = happyShift action_19
action_123 (53) = happyShift action_20
action_123 (56) = happyShift action_21
action_123 (58) = happyShift action_22
action_123 (19) = happyGoto action_126
action_123 (21) = happyGoto action_13
action_123 _ = happyFail

action_124 (25) = happyShift action_6
action_124 (32) = happyShift action_43
action_124 (35) = happyShift action_44
action_124 (21) = happyGoto action_42
action_124 _ = happyReduce_16

action_125 _ = happyReduce_32

action_126 (25) = happyShift action_6
action_126 (32) = happyShift action_43
action_126 (35) = happyShift action_44
action_126 (21) = happyGoto action_42
action_126 _ = happyReduce_23

action_127 (25) = happyShift action_6
action_127 (35) = happyShift action_14
action_127 (40) = happyShift action_15
action_127 (41) = happyShift action_16
action_127 (43) = happyShift action_17
action_127 (44) = happyShift action_18
action_127 (45) = happyShift action_19
action_127 (53) = happyShift action_20
action_127 (56) = happyShift action_21
action_127 (58) = happyShift action_22
action_127 (19) = happyGoto action_130
action_127 (21) = happyGoto action_13
action_127 _ = happyFail

action_128 _ = happyReduce_14

action_129 _ = happyReduce_9

action_130 (25) = happyShift action_6
action_130 (32) = happyShift action_43
action_130 (35) = happyShift action_44
action_130 (21) = happyGoto action_42
action_130 _ = happyReduce_47

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn7
		 (TyBase happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 5 7 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TyTuple (happy_var_2:happy_var_4)
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (TyFun happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 9 happyReduction_10
happyReduction_10 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  10 happyReduction_12
happyReduction_12 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 (HappyTerminal (TokenName happy_var_3))
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  12 happyReduction_15
happyReduction_15 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  12 happyReduction_16
happyReduction_16 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  13 happyReduction_18
happyReduction_18 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  14 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happyReduce 5 14 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ("tuple":happy_var_2:happy_var_4
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_2  15 happyReduction_21
happyReduction_21 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn15
		 ([happy_var_2]
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  15 happyReduction_22
happyReduction_22 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  16 happyReduction_23
happyReduction_23 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn16
		 (TAlt happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 5 17 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (map (\n -> TBind n happy_var_4) happy_var_2
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_1  18 happyReduction_25
happyReduction_25 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  18 happyReduction_26
happyReduction_26 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 ++ happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  19 happyReduction_27
happyReduction_27 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn19
		 (TVar happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  19 happyReduction_28
happyReduction_28 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (TUnr happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  19 happyReduction_29
happyReduction_29 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (TApp happy_var_1 (TVar happy_var_2)
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happyReduce 4 19 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_3  19 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happyReduce 6 19 happyReduction_32
happyReduction_32 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TApp happy_var_1 (TTuple (happy_var_3:happy_var_5))
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 5 19 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TTuple (happy_var_2:happy_var_4)
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 4 19 happyReduction_34
happyReduction_34 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_35 = happyReduce 4 19 happyReduction_35
happyReduction_35 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TFix happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_3  19 happyReduction_36
happyReduction_36 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (TEql happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happyReduce 6 19 happyReduction_37
happyReduction_37 ((HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TLet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_3  19 happyReduction_38
happyReduction_38 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (TFold happy_var_2
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happyReduce 5 19 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TCase happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 6 19 happyReduction_40
happyReduction_40 ((HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TAssert happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 6 19 happyReduction_41
happyReduction_41 ((HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TCase happy_var_2 [ TAlt ["True"] happy_var_4
                                                 , TAlt ["False"] happy_var_6]
	) `HappyStk` happyRest

happyReduce_42 = happyReduce 4 20 happyReduction_42
happyReduction_42 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_1  21 happyReduction_43
happyReduction_43 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn21
		 ((happy_var_1, [])
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happyReduce 4 21 happyReduction_44
happyReduction_44 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_45 = happySpecReduce_1  22 happyReduction_45
happyReduction_45 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn22
		 ((happy_var_1, [])
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happyReduce 4 22 happyReduction_46
happyReduction_46 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_47 = happyReduce 7 23 happyReduction_47
happyReduction_47 ((HappyAbsSyn19  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((happy_var_2, happy_var_5, happy_var_7)
	) `HappyStk` happyRest

happyReduce_48 = happyReduce 4 23 happyReduction_48
happyReduction_48 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((happy_var_2, [], happy_var_4)
	) `HappyStk` happyRest

happyReduce_49 = happySpecReduce_0  24 happyReduction_49
happyReduction_49  =  HappyAbsSyn24
		 (RawProgram [] [] []
	)

happyReduce_50 = happySpecReduce_2  24 happyReduction_50
happyReduction_50 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (modify programTypes (++ [happy_var_2]) happy_var_1
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_2  24 happyReduction_51
happyReduction_51 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (modify programTerms (++ [happy_var_2]) happy_var_1
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_2  24 happyReduction_52
happyReduction_52 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (modify programProps (++ [happy_var_2]) happy_var_1
	)
happyReduction_52 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 59 59 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 25;
	TokenBar -> cont 26;
	TokenTypeOf -> cont 27;
	TokenRArr -> cont 28;
	TokenLArr -> cont 29;
	TokenDRArr -> cont 30;
	TokenSet -> cont 31;
	TokenEq -> cont 32;
	TokenOS -> cont 33;
	TokenCS -> cont 34;
	TokenOP -> cont 35;
	TokenCP -> cont 36;
	TokenOA -> cont 37;
	TokenCA -> cont 38;
	TokenComma -> cont 39;
	TokenUnr -> cont 40;
	TokenMatch -> cont 41;
	TokenWith -> cont 42;
	TokenFun -> cont 43;
	TokenFix -> cont 44;
	TokenLet -> cont 45;
	TokenIn -> cont 46;
	TokenType -> cont 47;
	TokenEnd -> cont 48;
	TokenInj happy_dollar_dollar -> cont 49;
	TokenInd -> cont 50;
	TokenProp -> cont 51;
	TokenAll -> cont 52;
	TokenIf -> cont 53;
	TokenThen -> cont 54;
	TokenElse -> cont 55;
	TokenFold -> cont 56;
	TokenEqEq -> cont 57;
	TokenAssert -> cont 58;
	_ -> happyError' (tk:tks)
	}

happyError_ 59 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure    = return
    a <*> b = (fmap id a) <*> b
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . happyError

happyProgram tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

happyTerm tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

happyType tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

happyBindings tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


withEmptyScope :: ReaderT Scope m a -> m a
withEmptyScope = flip runReaderT (Scope mempty mempty mempty)

instance Monad m => Env.Write (ReaderT Scope m) where
  bindAt at b = 
      local
    $ modify bindMap (addToMap . map (Indices.liftAt at))
    . modify bindStack addToStack
    where
    addToStack = insertAt (enum at) b
    addToMap = Map.insert (get bindLabel b) (Var at)
    
  matched _ _ = id
  
instance Err.Throws m => Env.Read (ReaderT Scope m) where
  bindings = asks (get bindStack)
  
type ParserMonad m a = (Err.Throws m, Defs.Has m) => ReaderT Scope m a

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
  
term :: (Err.Throws m, Defs.Has m, Env.Read m) => String -> m Term
term str = do
  bs <- Env.bindings
  withEmptyScope
    . liftM Eval.run
    . Env.bindMany (reverse bs)
    . parseAndCheckTerm 
    . happyTerm 
    . lexer
    $ str
  
_type :: (Err.Throws m, Defs.Has m) => String -> m Type
_type = id
  . withEmptyScope
  . parseRawType
  . happyType 
  . lexer
  
bindings :: (Err.Throws m, Defs.Has m) => String -> m [Bind]
bindings = id
  . withEmptyScope
  . mapM parseRawBind
  . happyBindings
  . lexer
  
program :: forall m . (Err.Throws m, Defs.Has m) 
  => String -> m [Polymorphic Equation]
program text = 
  withEmptyScope $ do
    mapM_ defineType types
    mapM_ defineTerm terms
    mapM parseProp props
  where
  RawProgram types terms props = happyProgram (lexer text)
    
  parseProp :: PropDef -> ParserMonad m (Polymorphic Equation)
  parseProp ((name, ty_args), rbs, rt) = 
    localTypeArgs ty_args $ do
      bs <- mapM parseRawBind rbs
      t <- Env.bindMany bs (parseAndCheckTerm rt)
      if isQuantifiedEql t
      then return (Equals name bs t)
      else return (Equals name bs (Eql t Term.true))
  
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
      poly_con = fmap (\ind -> Con (Constructor ind (enum n))) poly_ind
      name = head (raw_cons !! n)
  
  defineTerm ((name, ty_args), raw_term) = do
    p_term <- localTypeArgs ty_args (parseAndCheckTerm raw_term)
    Defs.defineTerm name (fmap Simp.run p_term)
    
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
parseAndCheckTerm = id
  . Err.check Type.check 
  . parseRawTerm
  
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
parsePattern ind (con_lbl:var_lbls) 
  | null mby_con_i = 
    Err.throw 
      $ "Invalid constructor \"" ++ con_lbl 
      ++ "\" for type [" ++ show ind ++ "]"
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
  Fun (Base ind) res_ty <- parseRawType raw_ty
  return (buildFold ind res_ty)
parseRawTerm (TEql rt1 rt2) = do
  t1 <- parseRawTerm rt1
  t2 <- parseRawTerm rt2
  return (Term.Eql t1 t2)
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
  (con_n, var_bs) <- parsePattern ind pat
  in_t <- Env.bindMany var_bs (parseRawTerm rin_t)
  in_ty <- Env.bindMany var_bs (Type.getM in_t)
  let assrt = Constraint.make (Constructor ind con_n) on_t
  return (Constraint.apply assrt (in_t, in_ty))
parseRawTerm (TCase rt ralts) = do
  t <- parseRawTerm rt
  ind_ty <- Type.getM t
  alts <- mapM (parseRawAlt ind_ty) ralts
  let alts' = map snd (sortBy (compare `on` fst) alts)
  return (Case t alts')
  where
  parseRawAlt ind_ty (TAlt pat ralt_t)
    | not (Type.isInd ind_ty) =
      Err.throw 
        $ "Pattern matching over non inductive type [" ++ show ind_ty ++ "]"
    | otherwise = do
      (con_n, var_bs) <- parsePattern ind pat
      t <- Env.bindMany var_bs (parseRawTerm ralt_t)
      let con = Constructor ind con_n
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
  show TokenEqEq = "eq["
  show (TokenInj n) = "inj" ++ show n
  show TokenAssert = "assert"
  
  showList = (++) . intercalate " " . map show
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}

{-# LINE 46 "templates\\GenericTemplate.hs" #-}








{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
