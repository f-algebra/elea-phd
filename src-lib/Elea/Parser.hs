{-# OPTIONS_GHC -w #-}
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
import qualified Elea.Terms as Term
import qualified Elea.Types as Type
import qualified Elea.Constraint as Constraint
import qualified Elea.Monad.Env as Env
import qualified Elea.Foldable as Fold
import qualified Elea.Simplifier as Simp
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
  | TAbsurd RawType
  | TCase RawTerm [RawAlt]
  | TLet String RawTerm RawTerm
  | TEq RawType
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

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn6 (RawType)
	| HappyAbsSyn7 ([[String]])
	| HappyAbsSyn8 (TypeDef)
	| HappyAbsSyn9 ([String])
	| HappyAbsSyn11 ([RawTerm])
	| HappyAbsSyn12 ([RawType])
	| HappyAbsSyn14 ([RawAlt])
	| HappyAbsSyn15 (RawAlt)
	| HappyAbsSyn16 ([RawBind])
	| HappyAbsSyn18 (RawTerm)
	| HappyAbsSyn19 (TermDef)
	| HappyAbsSyn20 (ParamCall)
	| HappyAbsSyn21 (ParamName)
	| HappyAbsSyn22 ((RawTerm, RawTerm))
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
 action_130,
 action_131,
 action_132 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_3,
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
 happyReduce_52,
 happyReduce_53 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (24) = happyGoto action_20
action_0 _ = happyReduce_50

action_1 (25) = happyShift action_5
action_1 (35) = happyShift action_10
action_1 (40) = happyShift action_11
action_1 (41) = happyShift action_12
action_1 (43) = happyShift action_13
action_1 (44) = happyShift action_14
action_1 (45) = happyShift action_15
action_1 (53) = happyShift action_16
action_1 (56) = happyShift action_17
action_1 (57) = happyShift action_18
action_1 (58) = happyShift action_19
action_1 (18) = happyGoto action_8
action_1 (20) = happyGoto action_9
action_1 _ = happyFail

action_2 (25) = happyShift action_5
action_2 (35) = happyShift action_7
action_2 (6) = happyGoto action_6
action_2 (20) = happyGoto action_4
action_2 _ = happyFail

action_3 (25) = happyShift action_5
action_3 (20) = happyGoto action_4
action_3 _ = happyFail

action_4 _ = happyReduce_3

action_5 (37) = happyShift action_46
action_5 _ = happyReduce_42

action_6 (28) = happyShift action_45
action_6 (59) = happyAccept
action_6 _ = happyFail

action_7 (25) = happyShift action_5
action_7 (35) = happyShift action_7
action_7 (6) = happyGoto action_44
action_7 (20) = happyGoto action_4
action_7 _ = happyFail

action_8 (25) = happyShift action_5
action_8 (35) = happyShift action_43
action_8 (59) = happyAccept
action_8 (20) = happyGoto action_42
action_8 _ = happyFail

action_9 _ = happyReduce_26

action_10 (25) = happyShift action_5
action_10 (35) = happyShift action_10
action_10 (40) = happyShift action_11
action_10 (41) = happyShift action_12
action_10 (43) = happyShift action_13
action_10 (44) = happyShift action_14
action_10 (45) = happyShift action_15
action_10 (53) = happyShift action_16
action_10 (56) = happyShift action_17
action_10 (57) = happyShift action_18
action_10 (58) = happyShift action_19
action_10 (18) = happyGoto action_41
action_10 (20) = happyGoto action_9
action_10 _ = happyFail

action_11 (25) = happyShift action_5
action_11 (35) = happyShift action_7
action_11 (6) = happyGoto action_40
action_11 (20) = happyGoto action_4
action_11 _ = happyFail

action_12 (25) = happyShift action_5
action_12 (35) = happyShift action_10
action_12 (40) = happyShift action_11
action_12 (41) = happyShift action_12
action_12 (43) = happyShift action_13
action_12 (44) = happyShift action_14
action_12 (45) = happyShift action_15
action_12 (53) = happyShift action_16
action_12 (56) = happyShift action_17
action_12 (57) = happyShift action_18
action_12 (58) = happyShift action_19
action_12 (18) = happyGoto action_39
action_12 (20) = happyGoto action_9
action_12 _ = happyFail

action_13 (35) = happyShift action_37
action_13 (16) = happyGoto action_35
action_13 (17) = happyGoto action_38
action_13 _ = happyFail

action_14 (35) = happyShift action_37
action_14 (16) = happyGoto action_35
action_14 (17) = happyGoto action_36
action_14 _ = happyFail

action_15 (25) = happyShift action_34
action_15 _ = happyFail

action_16 (25) = happyShift action_5
action_16 (35) = happyShift action_10
action_16 (40) = happyShift action_11
action_16 (41) = happyShift action_12
action_16 (43) = happyShift action_13
action_16 (44) = happyShift action_14
action_16 (45) = happyShift action_15
action_16 (53) = happyShift action_16
action_16 (56) = happyShift action_17
action_16 (57) = happyShift action_18
action_16 (58) = happyShift action_19
action_16 (18) = happyGoto action_33
action_16 (20) = happyGoto action_9
action_16 _ = happyFail

action_17 (25) = happyShift action_5
action_17 (35) = happyShift action_7
action_17 (6) = happyGoto action_32
action_17 (20) = happyGoto action_4
action_17 _ = happyFail

action_18 (25) = happyShift action_5
action_18 (35) = happyShift action_7
action_18 (6) = happyGoto action_31
action_18 (20) = happyGoto action_4
action_18 _ = happyFail

action_19 (25) = happyShift action_29
action_19 (35) = happyShift action_30
action_19 (9) = happyGoto action_27
action_19 (13) = happyGoto action_28
action_19 _ = happyFail

action_20 (45) = happyShift action_24
action_20 (50) = happyShift action_25
action_20 (51) = happyShift action_26
action_20 (59) = happyAccept
action_20 (8) = happyGoto action_21
action_20 (19) = happyGoto action_22
action_20 (23) = happyGoto action_23
action_20 _ = happyFail

action_21 _ = happyReduce_51

action_22 _ = happyReduce_52

action_23 _ = happyReduce_53

action_24 (25) = happyShift action_68
action_24 (21) = happyGoto action_70
action_24 _ = happyFail

action_25 (25) = happyShift action_68
action_25 (21) = happyGoto action_69
action_25 _ = happyFail

action_26 (25) = happyShift action_68
action_26 (21) = happyGoto action_67
action_26 _ = happyFail

action_27 (25) = happyShift action_66
action_27 _ = happyReduce_18

action_28 (29) = happyShift action_65
action_28 _ = happyFail

action_29 _ = happyReduce_10

action_30 (25) = happyShift action_64
action_30 _ = happyFail

action_31 (28) = happyShift action_45
action_31 (34) = happyShift action_63
action_31 _ = happyFail

action_32 (28) = happyShift action_45
action_32 (34) = happyShift action_62
action_32 _ = happyFail

action_33 (25) = happyShift action_5
action_33 (35) = happyShift action_43
action_33 (54) = happyShift action_61
action_33 (20) = happyGoto action_42
action_33 _ = happyFail

action_34 (32) = happyShift action_60
action_34 _ = happyFail

action_35 _ = happyReduce_24

action_36 (28) = happyShift action_59
action_36 (35) = happyShift action_37
action_36 (16) = happyGoto action_56
action_36 _ = happyFail

action_37 (25) = happyShift action_29
action_37 (35) = happyShift action_30
action_37 (9) = happyGoto action_27
action_37 (13) = happyGoto action_58
action_37 _ = happyFail

action_38 (28) = happyShift action_57
action_38 (35) = happyShift action_37
action_38 (16) = happyGoto action_56
action_38 _ = happyFail

action_39 (25) = happyShift action_5
action_39 (35) = happyShift action_43
action_39 (42) = happyShift action_55
action_39 (20) = happyGoto action_42
action_39 _ = happyFail

action_40 (28) = happyShift action_45
action_40 _ = happyReduce_27

action_41 (25) = happyShift action_5
action_41 (35) = happyShift action_43
action_41 (36) = happyShift action_53
action_41 (39) = happyShift action_54
action_41 (20) = happyGoto action_42
action_41 _ = happyFail

action_42 _ = happyReduce_28

action_43 (25) = happyShift action_5
action_43 (35) = happyShift action_10
action_43 (40) = happyShift action_11
action_43 (41) = happyShift action_12
action_43 (43) = happyShift action_13
action_43 (44) = happyShift action_14
action_43 (45) = happyShift action_15
action_43 (53) = happyShift action_16
action_43 (56) = happyShift action_17
action_43 (57) = happyShift action_18
action_43 (58) = happyShift action_19
action_43 (18) = happyGoto action_52
action_43 (20) = happyGoto action_9
action_43 _ = happyFail

action_44 (28) = happyShift action_45
action_44 (36) = happyShift action_50
action_44 (39) = happyShift action_51
action_44 _ = happyFail

action_45 (25) = happyShift action_5
action_45 (35) = happyShift action_7
action_45 (6) = happyGoto action_49
action_45 (20) = happyGoto action_4
action_45 _ = happyFail

action_46 (25) = happyShift action_5
action_46 (35) = happyShift action_7
action_46 (6) = happyGoto action_47
action_46 (12) = happyGoto action_48
action_46 (20) = happyGoto action_4
action_46 _ = happyFail

action_47 (28) = happyShift action_45
action_47 _ = happyReduce_16

action_48 (38) = happyShift action_89
action_48 (39) = happyShift action_90
action_48 _ = happyFail

action_49 (28) = happyShift action_45
action_49 _ = happyReduce_6

action_50 _ = happyReduce_4

action_51 (25) = happyShift action_5
action_51 (35) = happyShift action_7
action_51 (6) = happyGoto action_47
action_51 (12) = happyGoto action_88
action_51 (20) = happyGoto action_4
action_51 _ = happyFail

action_52 (25) = happyShift action_5
action_52 (35) = happyShift action_43
action_52 (36) = happyShift action_86
action_52 (39) = happyShift action_87
action_52 (20) = happyGoto action_42
action_52 _ = happyFail

action_53 _ = happyReduce_30

action_54 (25) = happyShift action_5
action_54 (35) = happyShift action_10
action_54 (40) = happyShift action_11
action_54 (41) = happyShift action_12
action_54 (43) = happyShift action_13
action_54 (44) = happyShift action_14
action_54 (45) = happyShift action_15
action_54 (53) = happyShift action_16
action_54 (56) = happyShift action_17
action_54 (57) = happyShift action_18
action_54 (58) = happyShift action_19
action_54 (11) = happyGoto action_84
action_54 (18) = happyGoto action_85
action_54 (20) = happyGoto action_9
action_54 _ = happyFail

action_55 (26) = happyShift action_83
action_55 (14) = happyGoto action_82
action_55 _ = happyFail

action_56 _ = happyReduce_25

action_57 (25) = happyShift action_5
action_57 (35) = happyShift action_10
action_57 (40) = happyShift action_11
action_57 (41) = happyShift action_12
action_57 (43) = happyShift action_13
action_57 (44) = happyShift action_14
action_57 (45) = happyShift action_15
action_57 (53) = happyShift action_16
action_57 (56) = happyShift action_17
action_57 (57) = happyShift action_18
action_57 (58) = happyShift action_19
action_57 (18) = happyGoto action_81
action_57 (20) = happyGoto action_9
action_57 _ = happyFail

action_58 (27) = happyShift action_80
action_58 _ = happyFail

action_59 (25) = happyShift action_5
action_59 (35) = happyShift action_10
action_59 (40) = happyShift action_11
action_59 (41) = happyShift action_12
action_59 (43) = happyShift action_13
action_59 (44) = happyShift action_14
action_59 (45) = happyShift action_15
action_59 (53) = happyShift action_16
action_59 (56) = happyShift action_17
action_59 (57) = happyShift action_18
action_59 (58) = happyShift action_19
action_59 (18) = happyGoto action_79
action_59 (20) = happyGoto action_9
action_59 _ = happyFail

action_60 (25) = happyShift action_5
action_60 (35) = happyShift action_10
action_60 (40) = happyShift action_11
action_60 (41) = happyShift action_12
action_60 (43) = happyShift action_13
action_60 (44) = happyShift action_14
action_60 (45) = happyShift action_15
action_60 (53) = happyShift action_16
action_60 (56) = happyShift action_17
action_60 (57) = happyShift action_18
action_60 (58) = happyShift action_19
action_60 (18) = happyGoto action_78
action_60 (20) = happyGoto action_9
action_60 _ = happyFail

action_61 (25) = happyShift action_5
action_61 (35) = happyShift action_10
action_61 (40) = happyShift action_11
action_61 (41) = happyShift action_12
action_61 (43) = happyShift action_13
action_61 (44) = happyShift action_14
action_61 (45) = happyShift action_15
action_61 (53) = happyShift action_16
action_61 (56) = happyShift action_17
action_61 (57) = happyShift action_18
action_61 (58) = happyShift action_19
action_61 (18) = happyGoto action_77
action_61 (20) = happyGoto action_9
action_61 _ = happyFail

action_62 _ = happyReduce_37

action_63 _ = happyReduce_36

action_64 (39) = happyShift action_76
action_64 _ = happyFail

action_65 (25) = happyShift action_5
action_65 (35) = happyShift action_10
action_65 (40) = happyShift action_11
action_65 (41) = happyShift action_12
action_65 (43) = happyShift action_13
action_65 (44) = happyShift action_14
action_65 (45) = happyShift action_15
action_65 (53) = happyShift action_16
action_65 (56) = happyShift action_17
action_65 (57) = happyShift action_18
action_65 (58) = happyShift action_19
action_65 (18) = happyGoto action_75
action_65 (20) = happyGoto action_9
action_65 _ = happyFail

action_66 _ = happyReduce_11

action_67 (27) = happyShift action_74
action_67 _ = happyFail

action_68 (37) = happyShift action_73
action_68 _ = happyReduce_44

action_69 (32) = happyShift action_72
action_69 _ = happyFail

action_70 (32) = happyShift action_71
action_70 _ = happyFail

action_71 (25) = happyShift action_5
action_71 (35) = happyShift action_10
action_71 (40) = happyShift action_11
action_71 (41) = happyShift action_12
action_71 (43) = happyShift action_13
action_71 (44) = happyShift action_14
action_71 (45) = happyShift action_15
action_71 (53) = happyShift action_16
action_71 (56) = happyShift action_17
action_71 (57) = happyShift action_18
action_71 (58) = happyShift action_19
action_71 (18) = happyGoto action_112
action_71 (20) = happyGoto action_9
action_71 _ = happyFail

action_72 (25) = happyShift action_29
action_72 (35) = happyShift action_30
action_72 (7) = happyGoto action_110
action_72 (9) = happyGoto action_27
action_72 (13) = happyGoto action_111
action_72 _ = happyFail

action_73 (25) = happyShift action_104
action_73 (10) = happyGoto action_109
action_73 _ = happyFail

action_74 (25) = happyShift action_5
action_74 (35) = happyShift action_10
action_74 (40) = happyShift action_11
action_74 (41) = happyShift action_12
action_74 (43) = happyShift action_13
action_74 (44) = happyShift action_14
action_74 (45) = happyShift action_15
action_74 (52) = happyShift action_108
action_74 (53) = happyShift action_16
action_74 (56) = happyShift action_17
action_74 (57) = happyShift action_18
action_74 (58) = happyShift action_19
action_74 (18) = happyGoto action_106
action_74 (20) = happyGoto action_9
action_74 (22) = happyGoto action_107
action_74 _ = happyFail

action_75 (25) = happyShift action_5
action_75 (35) = happyShift action_43
action_75 (46) = happyShift action_105
action_75 (20) = happyGoto action_42
action_75 _ = happyFail

action_76 (25) = happyShift action_104
action_76 (10) = happyGoto action_103
action_76 _ = happyFail

action_77 (25) = happyShift action_5
action_77 (35) = happyShift action_43
action_77 (55) = happyShift action_102
action_77 (20) = happyGoto action_42
action_77 _ = happyFail

action_78 (25) = happyShift action_5
action_78 (35) = happyShift action_43
action_78 (46) = happyShift action_101
action_78 (20) = happyGoto action_42
action_78 _ = happyFail

action_79 (25) = happyShift action_5
action_79 (35) = happyShift action_43
action_79 (20) = happyGoto action_42
action_79 _ = happyReduce_34

action_80 (25) = happyShift action_5
action_80 (35) = happyShift action_7
action_80 (6) = happyGoto action_100
action_80 (20) = happyGoto action_4
action_80 _ = happyFail

action_81 (25) = happyShift action_5
action_81 (35) = happyShift action_43
action_81 (20) = happyGoto action_42
action_81 _ = happyReduce_33

action_82 (26) = happyShift action_98
action_82 (48) = happyShift action_99
action_82 _ = happyFail

action_83 (25) = happyShift action_29
action_83 (35) = happyShift action_30
action_83 (9) = happyGoto action_27
action_83 (13) = happyGoto action_96
action_83 (15) = happyGoto action_97
action_83 _ = happyFail

action_84 (36) = happyShift action_94
action_84 (39) = happyShift action_95
action_84 _ = happyFail

action_85 (25) = happyShift action_5
action_85 (35) = happyShift action_43
action_85 (20) = happyGoto action_42
action_85 _ = happyReduce_14

action_86 _ = happyReduce_29

action_87 (25) = happyShift action_5
action_87 (35) = happyShift action_10
action_87 (40) = happyShift action_11
action_87 (41) = happyShift action_12
action_87 (43) = happyShift action_13
action_87 (44) = happyShift action_14
action_87 (45) = happyShift action_15
action_87 (53) = happyShift action_16
action_87 (56) = happyShift action_17
action_87 (57) = happyShift action_18
action_87 (58) = happyShift action_19
action_87 (11) = happyGoto action_93
action_87 (18) = happyGoto action_85
action_87 (20) = happyGoto action_9
action_87 _ = happyFail

action_88 (36) = happyShift action_92
action_88 (39) = happyShift action_90
action_88 _ = happyFail

action_89 _ = happyReduce_43

action_90 (25) = happyShift action_5
action_90 (35) = happyShift action_7
action_90 (6) = happyGoto action_91
action_90 (20) = happyGoto action_4
action_90 _ = happyFail

action_91 (28) = happyShift action_45
action_91 _ = happyReduce_17

action_92 _ = happyReduce_5

action_93 (36) = happyShift action_126
action_93 (39) = happyShift action_95
action_93 _ = happyFail

action_94 _ = happyReduce_32

action_95 (25) = happyShift action_5
action_95 (35) = happyShift action_10
action_95 (40) = happyShift action_11
action_95 (41) = happyShift action_12
action_95 (43) = happyShift action_13
action_95 (44) = happyShift action_14
action_95 (45) = happyShift action_15
action_95 (53) = happyShift action_16
action_95 (56) = happyShift action_17
action_95 (57) = happyShift action_18
action_95 (58) = happyShift action_19
action_95 (18) = happyGoto action_125
action_95 (20) = happyGoto action_9
action_95 _ = happyFail

action_96 (28) = happyShift action_124
action_96 _ = happyFail

action_97 _ = happyReduce_20

action_98 (25) = happyShift action_29
action_98 (35) = happyShift action_30
action_98 (9) = happyGoto action_27
action_98 (13) = happyGoto action_96
action_98 (15) = happyGoto action_123
action_98 _ = happyFail

action_99 _ = happyReduce_38

action_100 (28) = happyShift action_45
action_100 (36) = happyShift action_122
action_100 _ = happyFail

action_101 (25) = happyShift action_5
action_101 (35) = happyShift action_10
action_101 (40) = happyShift action_11
action_101 (41) = happyShift action_12
action_101 (43) = happyShift action_13
action_101 (44) = happyShift action_14
action_101 (45) = happyShift action_15
action_101 (53) = happyShift action_16
action_101 (56) = happyShift action_17
action_101 (57) = happyShift action_18
action_101 (58) = happyShift action_19
action_101 (18) = happyGoto action_121
action_101 (20) = happyGoto action_9
action_101 _ = happyFail

action_102 (25) = happyShift action_5
action_102 (35) = happyShift action_10
action_102 (40) = happyShift action_11
action_102 (41) = happyShift action_12
action_102 (43) = happyShift action_13
action_102 (44) = happyShift action_14
action_102 (45) = happyShift action_15
action_102 (53) = happyShift action_16
action_102 (56) = happyShift action_17
action_102 (57) = happyShift action_18
action_102 (58) = happyShift action_19
action_102 (18) = happyGoto action_120
action_102 (20) = happyGoto action_9
action_102 _ = happyFail

action_103 (36) = happyShift action_119
action_103 (39) = happyShift action_115
action_103 _ = happyFail

action_104 _ = happyReduce_12

action_105 (25) = happyShift action_5
action_105 (35) = happyShift action_10
action_105 (40) = happyShift action_11
action_105 (41) = happyShift action_12
action_105 (43) = happyShift action_13
action_105 (44) = happyShift action_14
action_105 (45) = happyShift action_15
action_105 (53) = happyShift action_16
action_105 (56) = happyShift action_17
action_105 (57) = happyShift action_18
action_105 (58) = happyShift action_19
action_105 (18) = happyGoto action_118
action_105 (20) = happyGoto action_9
action_105 _ = happyFail

action_106 (25) = happyShift action_5
action_106 (32) = happyShift action_117
action_106 (35) = happyShift action_43
action_106 (20) = happyGoto action_42
action_106 _ = happyReduce_46

action_107 _ = happyReduce_49

action_108 (35) = happyShift action_37
action_108 (16) = happyGoto action_35
action_108 (17) = happyGoto action_116
action_108 _ = happyFail

action_109 (38) = happyShift action_114
action_109 (39) = happyShift action_115
action_109 _ = happyFail

action_110 (26) = happyShift action_113
action_110 _ = happyReduce_9

action_111 _ = happyReduce_7

action_112 (25) = happyShift action_5
action_112 (35) = happyShift action_43
action_112 (20) = happyGoto action_42
action_112 _ = happyReduce_41

action_113 (25) = happyShift action_29
action_113 (35) = happyShift action_30
action_113 (9) = happyGoto action_27
action_113 (13) = happyGoto action_131
action_113 _ = happyFail

action_114 _ = happyReduce_45

action_115 (25) = happyShift action_130
action_115 _ = happyFail

action_116 (28) = happyShift action_129
action_116 (35) = happyShift action_37
action_116 (16) = happyGoto action_56
action_116 _ = happyFail

action_117 (25) = happyShift action_5
action_117 (35) = happyShift action_10
action_117 (40) = happyShift action_11
action_117 (41) = happyShift action_12
action_117 (43) = happyShift action_13
action_117 (44) = happyShift action_14
action_117 (45) = happyShift action_15
action_117 (53) = happyShift action_16
action_117 (56) = happyShift action_17
action_117 (57) = happyShift action_18
action_117 (58) = happyShift action_19
action_117 (18) = happyGoto action_128
action_117 (20) = happyGoto action_9
action_117 _ = happyFail

action_118 (25) = happyShift action_5
action_118 (35) = happyShift action_43
action_118 (20) = happyGoto action_42
action_118 _ = happyReduce_39

action_119 _ = happyReduce_19

action_120 (25) = happyShift action_5
action_120 (35) = happyShift action_43
action_120 (20) = happyGoto action_42
action_120 _ = happyReduce_40

action_121 (25) = happyShift action_5
action_121 (35) = happyShift action_43
action_121 (20) = happyGoto action_42
action_121 _ = happyReduce_35

action_122 _ = happyReduce_23

action_123 _ = happyReduce_21

action_124 (25) = happyShift action_5
action_124 (35) = happyShift action_10
action_124 (40) = happyShift action_11
action_124 (41) = happyShift action_12
action_124 (43) = happyShift action_13
action_124 (44) = happyShift action_14
action_124 (45) = happyShift action_15
action_124 (53) = happyShift action_16
action_124 (56) = happyShift action_17
action_124 (57) = happyShift action_18
action_124 (58) = happyShift action_19
action_124 (18) = happyGoto action_127
action_124 (20) = happyGoto action_9
action_124 _ = happyFail

action_125 (25) = happyShift action_5
action_125 (35) = happyShift action_43
action_125 (20) = happyGoto action_42
action_125 _ = happyReduce_15

action_126 _ = happyReduce_31

action_127 (25) = happyShift action_5
action_127 (35) = happyShift action_43
action_127 (20) = happyGoto action_42
action_127 _ = happyReduce_22

action_128 (25) = happyShift action_5
action_128 (35) = happyShift action_43
action_128 (20) = happyGoto action_42
action_128 _ = happyReduce_47

action_129 (25) = happyShift action_5
action_129 (35) = happyShift action_10
action_129 (40) = happyShift action_11
action_129 (41) = happyShift action_12
action_129 (43) = happyShift action_13
action_129 (44) = happyShift action_14
action_129 (45) = happyShift action_15
action_129 (53) = happyShift action_16
action_129 (56) = happyShift action_17
action_129 (57) = happyShift action_18
action_129 (58) = happyShift action_19
action_129 (18) = happyGoto action_106
action_129 (20) = happyGoto action_9
action_129 (22) = happyGoto action_132
action_129 _ = happyFail

action_130 _ = happyReduce_13

action_131 _ = happyReduce_8

action_132 _ = happyReduce_48

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn6
		 (TyBase happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 5 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (TyTuple (happy_var_2:happy_var_4)
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (TyFun happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 8 happyReduction_9
happyReduction_9 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  9 happyReduction_11
happyReduction_11 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  10 happyReduction_13
happyReduction_13 (HappyTerminal (TokenName happy_var_3))
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  11 happyReduction_14
happyReduction_14 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  11 happyReduction_15
happyReduction_15 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  12 happyReduction_16
happyReduction_16 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  12 happyReduction_17
happyReduction_17 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  13 happyReduction_18
happyReduction_18 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happyReduce 5 13 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ("tuple":happy_var_2:happy_var_4
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_2  14 happyReduction_20
happyReduction_20 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn14
		 ([happy_var_2]
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  14 happyReduction_21
happyReduction_21 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  15 happyReduction_22
happyReduction_22 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn15
		 (TAlt happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 5 16 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (map (\n -> TBind n happy_var_4) happy_var_2
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_1  17 happyReduction_24
happyReduction_24 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  17 happyReduction_25
happyReduction_25 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  18 happyReduction_26
happyReduction_26 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn18
		 (TVar happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  18 happyReduction_27
happyReduction_27 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (TAbsurd happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  18 happyReduction_28
happyReduction_28 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (TApp happy_var_1 (TVar happy_var_2)
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happyReduce 4 18 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_3  18 happyReduction_30
happyReduction_30 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happyReduce 6 18 happyReduction_31
happyReduction_31 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TApp happy_var_1 (TTuple (happy_var_3:happy_var_5))
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 5 18 happyReduction_32
happyReduction_32 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TTuple (happy_var_2:happy_var_4)
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 4 18 happyReduction_33
happyReduction_33 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 4 18 happyReduction_34
happyReduction_34 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TFix happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_35 = happyReduce 6 18 happyReduction_35
happyReduction_35 ((HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TLet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_3  18 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (TEq happy_var_2
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  18 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (TFold happy_var_2
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happyReduce 5 18 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TCase happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 6 18 happyReduction_39
happyReduction_39 ((HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TAssert happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 6 18 happyReduction_40
happyReduction_40 ((HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TCase happy_var_2 [ TAlt ["True"] happy_var_4
                                                 , TAlt ["False"] happy_var_6]
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 4 19 happyReduction_41
happyReduction_41 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_1  20 happyReduction_42
happyReduction_42 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn20
		 ((happy_var_1, [])
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happyReduce 4 20 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_1  21 happyReduction_44
happyReduction_44 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn21
		 ((happy_var_1, [])
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happyReduce 4 21 happyReduction_45
happyReduction_45 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_1  22 happyReduction_46
happyReduction_46 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn22
		 ((happy_var_1, TVar ("True", []))
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  22 happyReduction_47
happyReduction_47 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn22
		 ((happy_var_1, happy_var_3)
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happyReduce 7 23 happyReduction_48
happyReduction_48 ((HappyAbsSyn22  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((happy_var_2, happy_var_5, happy_var_7)
	) `HappyStk` happyRest

happyReduce_49 = happyReduce 4 23 happyReduction_49
happyReduction_49 ((HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((happy_var_2, [], happy_var_4)
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_0  24 happyReduction_50
happyReduction_50  =  HappyAbsSyn24
		 (RawProgram [] [] []
	)

happyReduce_51 = happySpecReduce_2  24 happyReduction_51
happyReduction_51 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (modify programTypes (++ [happy_var_2]) happy_var_1
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_2  24 happyReduction_52
happyReduction_52 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (modify programTerms (++ [happy_var_2]) happy_var_1
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  24 happyReduction_53
happyReduction_53 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (modify programProps (++ [happy_var_2]) happy_var_1
	)
happyReduction_53 _ _  = notHappyAtAll 

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
	TokenAbsurd -> cont 40;
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
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

happyType tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

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
parseAndCheckTerm = 
  Err.check Type.check . parseRawTerm
  
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
  mby_con_i = findIndices ((== con_lbl) . get boundLabel) cons
  con_i = head mby_con_i
  this_con = cons !! con_i
  
  -- The bindings for this constructor are the arguments for the type
  con_tys = (init . Type.flatten . get boundType) this_con
  var_bs = zipWith Bind var_lbls con_tys
  

parseRawTerm :: RawTerm -> ParserMonad m Term
parseRawTerm (TTuple rts) = do
  ts <- mapM parseRawTerm rts
  Term.tuple ts
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
parseRawTerm (TAssert pat ron_t rin_t) = do
  on_t <- parseRawTerm ron_t
  on_ty@(Type.Base ind) <- Type.get on_t
  (con_n, var_bs) <- parsePattern ind pat
  in_t <- Env.bindMany var_bs (parseRawTerm rin_t)
  in_ty <- Env.bindMany var_bs (Type.get in_t)
  let assrt = Constraint.make on_t ind con_n
  return (Constraint.apply assrt (in_t, in_ty))
parseRawTerm (TCase rt ralts) = do
  t <- parseRawTerm rt
  ind_ty <- Type.get t
  alts <- mapM (parseRawAlt ind_ty) ralts
  let alts' = map snd (sortBy (compare `on` fst) alts)
  return (Case (Type.inductiveType ind_ty) t alts')
  where
  parseRawAlt ind_ty (TAlt pat ralt_t)
    | not (Type.isInd ind_ty) =
      Err.throw 
        $ "Pattern matching over non inductive type [" ++ show ind_ty ++ "]"
    | otherwise = do
      (con_n, var_bs) <- parsePattern (Type.inductiveType ind_ty) pat
      t <- Env.bindMany var_bs (parseRawTerm ralt_t)
      return (con_n, Alt var_bs t)

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

{-# LINE 30 "templates\\GenericTemplate.hs" #-}








{-# LINE 51 "templates\\GenericTemplate.hs" #-}

{-# LINE 61 "templates\\GenericTemplate.hs" #-}

{-# LINE 70 "templates\\GenericTemplate.hs" #-}

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

{-# LINE 148 "templates\\GenericTemplate.hs" #-}

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
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
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
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
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
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates\\GenericTemplate.hs" #-}
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
