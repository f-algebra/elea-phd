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

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn6 (RawType)
	| HappyAbsSyn7 ([[String]])
	| HappyAbsSyn8 (TypeDef)
	| HappyAbsSyn9 ([String])
	| HappyAbsSyn10 ([RawAlt])
	| HappyAbsSyn11 (RawAlt)
	| HappyAbsSyn12 ([RawBind])
	| HappyAbsSyn14 (RawTerm)
	| HappyAbsSyn15 (TermDef)
	| HappyAbsSyn16 ([RawType])
	| HappyAbsSyn17 (ParamCall)
	| HappyAbsSyn19 (ParamName)
	| HappyAbsSyn20 ((RawTerm, RawTerm))
	| HappyAbsSyn21 (PropDef)
	| HappyAbsSyn22 (RawProgram)

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
 action_108 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_47 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (22) = happyGoto action_19
action_0 _ = happyReduce_44

action_1 (23) = happyShift action_5
action_1 (32) = happyShift action_10
action_1 (37) = happyShift action_11
action_1 (38) = happyShift action_12
action_1 (40) = happyShift action_13
action_1 (41) = happyShift action_14
action_1 (42) = happyShift action_15
action_1 (50) = happyShift action_16
action_1 (53) = happyShift action_17
action_1 (54) = happyShift action_18
action_1 (14) = happyGoto action_8
action_1 (17) = happyGoto action_9
action_1 _ = happyFail

action_2 (23) = happyShift action_5
action_2 (32) = happyShift action_7
action_2 (6) = happyGoto action_6
action_2 (17) = happyGoto action_4
action_2 _ = happyFail

action_3 (23) = happyShift action_5
action_3 (17) = happyGoto action_4
action_3 _ = happyFail

action_4 _ = happyReduce_3

action_5 (34) = happyShift action_41
action_5 _ = happyReduce_33

action_6 (26) = happyShift action_40
action_6 (55) = happyAccept
action_6 _ = happyFail

action_7 (23) = happyShift action_5
action_7 (32) = happyShift action_7
action_7 (6) = happyGoto action_39
action_7 (17) = happyGoto action_4
action_7 _ = happyFail

action_8 (23) = happyShift action_5
action_8 (32) = happyShift action_38
action_8 (55) = happyAccept
action_8 (17) = happyGoto action_37
action_8 _ = happyFail

action_9 _ = happyReduce_17

action_10 (23) = happyShift action_5
action_10 (32) = happyShift action_10
action_10 (37) = happyShift action_11
action_10 (38) = happyShift action_12
action_10 (40) = happyShift action_13
action_10 (41) = happyShift action_14
action_10 (42) = happyShift action_15
action_10 (50) = happyShift action_16
action_10 (53) = happyShift action_17
action_10 (54) = happyShift action_18
action_10 (14) = happyGoto action_36
action_10 (17) = happyGoto action_9
action_10 _ = happyFail

action_11 (23) = happyShift action_5
action_11 (32) = happyShift action_7
action_11 (6) = happyGoto action_35
action_11 (17) = happyGoto action_4
action_11 _ = happyFail

action_12 (23) = happyShift action_5
action_12 (32) = happyShift action_10
action_12 (37) = happyShift action_11
action_12 (38) = happyShift action_12
action_12 (40) = happyShift action_13
action_12 (41) = happyShift action_14
action_12 (42) = happyShift action_15
action_12 (50) = happyShift action_16
action_12 (53) = happyShift action_17
action_12 (54) = happyShift action_18
action_12 (14) = happyGoto action_34
action_12 (17) = happyGoto action_9
action_12 _ = happyFail

action_13 (32) = happyShift action_32
action_13 (12) = happyGoto action_30
action_13 (13) = happyGoto action_33
action_13 _ = happyFail

action_14 (32) = happyShift action_32
action_14 (12) = happyGoto action_30
action_14 (13) = happyGoto action_31
action_14 _ = happyFail

action_15 (23) = happyShift action_29
action_15 _ = happyFail

action_16 (23) = happyShift action_5
action_16 (32) = happyShift action_10
action_16 (37) = happyShift action_11
action_16 (38) = happyShift action_12
action_16 (40) = happyShift action_13
action_16 (41) = happyShift action_14
action_16 (42) = happyShift action_15
action_16 (50) = happyShift action_16
action_16 (53) = happyShift action_17
action_16 (54) = happyShift action_18
action_16 (14) = happyGoto action_28
action_16 (17) = happyGoto action_9
action_16 _ = happyFail

action_17 (23) = happyShift action_5
action_17 (32) = happyShift action_7
action_17 (6) = happyGoto action_27
action_17 (17) = happyGoto action_4
action_17 _ = happyFail

action_18 (23) = happyShift action_5
action_18 (32) = happyShift action_7
action_18 (6) = happyGoto action_26
action_18 (17) = happyGoto action_4
action_18 _ = happyFail

action_19 (42) = happyShift action_23
action_19 (47) = happyShift action_24
action_19 (48) = happyShift action_25
action_19 (55) = happyAccept
action_19 (8) = happyGoto action_20
action_19 (15) = happyGoto action_21
action_19 (21) = happyGoto action_22
action_19 _ = happyFail

action_20 _ = happyReduce_45

action_21 _ = happyReduce_46

action_22 _ = happyReduce_47

action_23 (23) = happyShift action_59
action_23 (19) = happyGoto action_61
action_23 _ = happyFail

action_24 (23) = happyShift action_59
action_24 (19) = happyGoto action_60
action_24 _ = happyFail

action_25 (23) = happyShift action_59
action_25 (19) = happyGoto action_58
action_25 _ = happyFail

action_26 (26) = happyShift action_40
action_26 (31) = happyShift action_57
action_26 _ = happyFail

action_27 (26) = happyShift action_40
action_27 (31) = happyShift action_56
action_27 _ = happyFail

action_28 (23) = happyShift action_5
action_28 (32) = happyShift action_38
action_28 (51) = happyShift action_55
action_28 (17) = happyGoto action_37
action_28 _ = happyFail

action_29 (29) = happyShift action_54
action_29 _ = happyFail

action_30 _ = happyReduce_15

action_31 (26) = happyShift action_53
action_31 (32) = happyShift action_32
action_31 (12) = happyGoto action_49
action_31 _ = happyFail

action_32 (23) = happyShift action_52
action_32 (9) = happyGoto action_51
action_32 _ = happyFail

action_33 (26) = happyShift action_50
action_33 (32) = happyShift action_32
action_33 (12) = happyGoto action_49
action_33 _ = happyFail

action_34 (23) = happyShift action_5
action_34 (32) = happyShift action_38
action_34 (39) = happyShift action_48
action_34 (17) = happyGoto action_37
action_34 _ = happyFail

action_35 (26) = happyShift action_40
action_35 _ = happyReduce_18

action_36 (23) = happyShift action_5
action_36 (32) = happyShift action_38
action_36 (33) = happyShift action_47
action_36 (17) = happyGoto action_37
action_36 _ = happyFail

action_37 _ = happyReduce_19

action_38 (23) = happyShift action_5
action_38 (32) = happyShift action_10
action_38 (37) = happyShift action_11
action_38 (38) = happyShift action_12
action_38 (40) = happyShift action_13
action_38 (41) = happyShift action_14
action_38 (42) = happyShift action_15
action_38 (50) = happyShift action_16
action_38 (53) = happyShift action_17
action_38 (54) = happyShift action_18
action_38 (14) = happyGoto action_46
action_38 (17) = happyGoto action_9
action_38 _ = happyFail

action_39 (26) = happyShift action_40
action_39 (33) = happyShift action_45
action_39 _ = happyFail

action_40 (23) = happyShift action_5
action_40 (32) = happyShift action_7
action_40 (6) = happyGoto action_44
action_40 (17) = happyGoto action_4
action_40 _ = happyFail

action_41 (23) = happyShift action_5
action_41 (32) = happyShift action_7
action_41 (6) = happyGoto action_42
action_41 (16) = happyGoto action_43
action_41 (17) = happyGoto action_4
action_41 _ = happyReduce_30

action_42 (26) = happyShift action_40
action_42 _ = happyReduce_31

action_43 (35) = happyShift action_75
action_43 (36) = happyShift action_76
action_43 _ = happyFail

action_44 (26) = happyShift action_40
action_44 _ = happyReduce_5

action_45 _ = happyReduce_4

action_46 (23) = happyShift action_5
action_46 (32) = happyShift action_38
action_46 (33) = happyShift action_74
action_46 (17) = happyGoto action_37
action_46 _ = happyFail

action_47 _ = happyReduce_21

action_48 (24) = happyShift action_73
action_48 (10) = happyGoto action_72
action_48 _ = happyFail

action_49 _ = happyReduce_16

action_50 (23) = happyShift action_5
action_50 (32) = happyShift action_10
action_50 (37) = happyShift action_11
action_50 (38) = happyShift action_12
action_50 (40) = happyShift action_13
action_50 (41) = happyShift action_14
action_50 (42) = happyShift action_15
action_50 (50) = happyShift action_16
action_50 (53) = happyShift action_17
action_50 (54) = happyShift action_18
action_50 (14) = happyGoto action_71
action_50 (17) = happyGoto action_9
action_50 _ = happyFail

action_51 (23) = happyShift action_69
action_51 (25) = happyShift action_70
action_51 _ = happyFail

action_52 _ = happyReduce_9

action_53 (23) = happyShift action_5
action_53 (32) = happyShift action_10
action_53 (37) = happyShift action_11
action_53 (38) = happyShift action_12
action_53 (40) = happyShift action_13
action_53 (41) = happyShift action_14
action_53 (42) = happyShift action_15
action_53 (50) = happyShift action_16
action_53 (53) = happyShift action_17
action_53 (54) = happyShift action_18
action_53 (14) = happyGoto action_68
action_53 (17) = happyGoto action_9
action_53 _ = happyFail

action_54 (23) = happyShift action_5
action_54 (32) = happyShift action_10
action_54 (37) = happyShift action_11
action_54 (38) = happyShift action_12
action_54 (40) = happyShift action_13
action_54 (41) = happyShift action_14
action_54 (42) = happyShift action_15
action_54 (50) = happyShift action_16
action_54 (53) = happyShift action_17
action_54 (54) = happyShift action_18
action_54 (14) = happyGoto action_67
action_54 (17) = happyGoto action_9
action_54 _ = happyFail

action_55 (23) = happyShift action_5
action_55 (32) = happyShift action_10
action_55 (37) = happyShift action_11
action_55 (38) = happyShift action_12
action_55 (40) = happyShift action_13
action_55 (41) = happyShift action_14
action_55 (42) = happyShift action_15
action_55 (50) = happyShift action_16
action_55 (53) = happyShift action_17
action_55 (54) = happyShift action_18
action_55 (14) = happyGoto action_66
action_55 (17) = happyGoto action_9
action_55 _ = happyFail

action_56 _ = happyReduce_26

action_57 _ = happyReduce_25

action_58 (25) = happyShift action_65
action_58 _ = happyFail

action_59 (34) = happyShift action_64
action_59 _ = happyReduce_38

action_60 (29) = happyShift action_63
action_60 _ = happyFail

action_61 (29) = happyShift action_62
action_61 _ = happyFail

action_62 (23) = happyShift action_5
action_62 (32) = happyShift action_10
action_62 (37) = happyShift action_11
action_62 (38) = happyShift action_12
action_62 (40) = happyShift action_13
action_62 (41) = happyShift action_14
action_62 (42) = happyShift action_15
action_62 (50) = happyShift action_16
action_62 (53) = happyShift action_17
action_62 (54) = happyShift action_18
action_62 (14) = happyGoto action_92
action_62 (17) = happyGoto action_9
action_62 _ = happyFail

action_63 (23) = happyShift action_52
action_63 (7) = happyGoto action_90
action_63 (9) = happyGoto action_91
action_63 _ = happyFail

action_64 (23) = happyShift action_89
action_64 (18) = happyGoto action_88
action_64 _ = happyReduce_35

action_65 (23) = happyShift action_5
action_65 (32) = happyShift action_10
action_65 (37) = happyShift action_11
action_65 (38) = happyShift action_12
action_65 (40) = happyShift action_13
action_65 (41) = happyShift action_14
action_65 (42) = happyShift action_15
action_65 (49) = happyShift action_87
action_65 (50) = happyShift action_16
action_65 (53) = happyShift action_17
action_65 (54) = happyShift action_18
action_65 (14) = happyGoto action_85
action_65 (17) = happyGoto action_9
action_65 (20) = happyGoto action_86
action_65 _ = happyFail

action_66 (23) = happyShift action_5
action_66 (32) = happyShift action_38
action_66 (52) = happyShift action_84
action_66 (17) = happyGoto action_37
action_66 _ = happyFail

action_67 (23) = happyShift action_5
action_67 (32) = happyShift action_38
action_67 (43) = happyShift action_83
action_67 (17) = happyGoto action_37
action_67 _ = happyFail

action_68 (23) = happyShift action_5
action_68 (32) = happyShift action_38
action_68 (17) = happyGoto action_37
action_68 _ = happyReduce_23

action_69 _ = happyReduce_10

action_70 (23) = happyShift action_5
action_70 (32) = happyShift action_7
action_70 (6) = happyGoto action_82
action_70 (17) = happyGoto action_4
action_70 _ = happyFail

action_71 (23) = happyShift action_5
action_71 (32) = happyShift action_38
action_71 (17) = happyGoto action_37
action_71 _ = happyReduce_22

action_72 (24) = happyShift action_80
action_72 (45) = happyShift action_81
action_72 _ = happyFail

action_73 (23) = happyShift action_52
action_73 (9) = happyGoto action_78
action_73 (11) = happyGoto action_79
action_73 _ = happyFail

action_74 _ = happyReduce_20

action_75 _ = happyReduce_34

action_76 (23) = happyShift action_5
action_76 (32) = happyShift action_7
action_76 (6) = happyGoto action_77
action_76 (17) = happyGoto action_4
action_76 _ = happyFail

action_77 (26) = happyShift action_40
action_77 _ = happyReduce_32

action_78 (23) = happyShift action_69
action_78 (26) = happyShift action_102
action_78 _ = happyFail

action_79 _ = happyReduce_11

action_80 (23) = happyShift action_52
action_80 (9) = happyGoto action_78
action_80 (11) = happyGoto action_101
action_80 _ = happyFail

action_81 _ = happyReduce_27

action_82 (26) = happyShift action_40
action_82 (33) = happyShift action_100
action_82 _ = happyFail

action_83 (23) = happyShift action_5
action_83 (32) = happyShift action_10
action_83 (37) = happyShift action_11
action_83 (38) = happyShift action_12
action_83 (40) = happyShift action_13
action_83 (41) = happyShift action_14
action_83 (42) = happyShift action_15
action_83 (50) = happyShift action_16
action_83 (53) = happyShift action_17
action_83 (54) = happyShift action_18
action_83 (14) = happyGoto action_99
action_83 (17) = happyGoto action_9
action_83 _ = happyFail

action_84 (23) = happyShift action_5
action_84 (32) = happyShift action_10
action_84 (37) = happyShift action_11
action_84 (38) = happyShift action_12
action_84 (40) = happyShift action_13
action_84 (41) = happyShift action_14
action_84 (42) = happyShift action_15
action_84 (50) = happyShift action_16
action_84 (53) = happyShift action_17
action_84 (54) = happyShift action_18
action_84 (14) = happyGoto action_98
action_84 (17) = happyGoto action_9
action_84 _ = happyFail

action_85 (23) = happyShift action_5
action_85 (29) = happyShift action_97
action_85 (32) = happyShift action_38
action_85 (17) = happyGoto action_37
action_85 _ = happyReduce_40

action_86 _ = happyReduce_43

action_87 (32) = happyShift action_32
action_87 (12) = happyGoto action_30
action_87 (13) = happyGoto action_96
action_87 _ = happyFail

action_88 (35) = happyShift action_94
action_88 (36) = happyShift action_95
action_88 _ = happyFail

action_89 _ = happyReduce_36

action_90 (24) = happyShift action_93
action_90 _ = happyReduce_8

action_91 (23) = happyShift action_69
action_91 _ = happyReduce_6

action_92 (23) = happyShift action_5
action_92 (32) = happyShift action_38
action_92 (17) = happyGoto action_37
action_92 _ = happyReduce_29

action_93 (23) = happyShift action_52
action_93 (9) = happyGoto action_107
action_93 _ = happyFail

action_94 _ = happyReduce_39

action_95 (23) = happyShift action_106
action_95 _ = happyFail

action_96 (26) = happyShift action_105
action_96 (32) = happyShift action_32
action_96 (12) = happyGoto action_49
action_96 _ = happyFail

action_97 (23) = happyShift action_5
action_97 (32) = happyShift action_10
action_97 (37) = happyShift action_11
action_97 (38) = happyShift action_12
action_97 (40) = happyShift action_13
action_97 (41) = happyShift action_14
action_97 (42) = happyShift action_15
action_97 (50) = happyShift action_16
action_97 (53) = happyShift action_17
action_97 (54) = happyShift action_18
action_97 (14) = happyGoto action_104
action_97 (17) = happyGoto action_9
action_97 _ = happyFail

action_98 (23) = happyShift action_5
action_98 (32) = happyShift action_38
action_98 (17) = happyGoto action_37
action_98 _ = happyReduce_28

action_99 (23) = happyShift action_5
action_99 (32) = happyShift action_38
action_99 (17) = happyGoto action_37
action_99 _ = happyReduce_24

action_100 _ = happyReduce_14

action_101 _ = happyReduce_12

action_102 (23) = happyShift action_5
action_102 (32) = happyShift action_10
action_102 (37) = happyShift action_11
action_102 (38) = happyShift action_12
action_102 (40) = happyShift action_13
action_102 (41) = happyShift action_14
action_102 (42) = happyShift action_15
action_102 (50) = happyShift action_16
action_102 (53) = happyShift action_17
action_102 (54) = happyShift action_18
action_102 (14) = happyGoto action_103
action_102 (17) = happyGoto action_9
action_102 _ = happyFail

action_103 (23) = happyShift action_5
action_103 (32) = happyShift action_38
action_103 (17) = happyGoto action_37
action_103 _ = happyReduce_13

action_104 (23) = happyShift action_5
action_104 (32) = happyShift action_38
action_104 (17) = happyGoto action_37
action_104 _ = happyReduce_41

action_105 (23) = happyShift action_5
action_105 (32) = happyShift action_10
action_105 (37) = happyShift action_11
action_105 (38) = happyShift action_12
action_105 (40) = happyShift action_13
action_105 (41) = happyShift action_14
action_105 (42) = happyShift action_15
action_105 (50) = happyShift action_16
action_105 (53) = happyShift action_17
action_105 (54) = happyShift action_18
action_105 (14) = happyGoto action_85
action_105 (17) = happyGoto action_9
action_105 (20) = happyGoto action_108
action_105 _ = happyFail

action_106 _ = happyReduce_37

action_107 (23) = happyShift action_69
action_107 _ = happyReduce_7

action_108 _ = happyReduce_42

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn6
		 (TBase happy_var_1
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

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (TFun happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 8 happyReduction_8
happyReduction_8 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  10 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 ([happy_var_2]
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  11 happyReduction_13
happyReduction_13 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn11
		 (TAlt happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 5 12 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (map (\n -> TBind n happy_var_4) happy_var_2
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  13 happyReduction_15
happyReduction_15 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  13 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  14 happyReduction_17
happyReduction_17 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn14
		 (TVar happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  14 happyReduction_18
happyReduction_18 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (TAbsurd happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  14 happyReduction_19
happyReduction_19 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (TApp happy_var_1 (TVar happy_var_2)
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 14 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (TApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_3  14 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 4 14 happyReduction_22
happyReduction_22 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (TLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 4 14 happyReduction_23
happyReduction_23 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (TFix happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 6 14 happyReduction_24
happyReduction_24 ((HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (TLet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  14 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (TEq happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  14 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (TFold happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 5 14 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (TCase happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 6 14 happyReduction_28
happyReduction_28 ((HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (TCase happy_var_2 [ TAlt ["True"] happy_var_4
                                                 , TAlt ["False"] happy_var_6]
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 4 15 happyReduction_29
happyReduction_29 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_0  16 happyReduction_30
happyReduction_30  =  HappyAbsSyn16
		 ([]
	)

happyReduce_31 = happySpecReduce_1  16 happyReduction_31
happyReduction_31 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  16 happyReduction_32
happyReduction_32 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  17 happyReduction_33
happyReduction_33 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn17
		 ((happy_var_1, [])
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happyReduce 4 17 happyReduction_34
happyReduction_34 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_0  18 happyReduction_35
happyReduction_35  =  HappyAbsSyn9
		 ([]
	)

happyReduce_36 = happySpecReduce_1  18 happyReduction_36
happyReduction_36 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  18 happyReduction_37
happyReduction_37 (HappyTerminal (TokenName happy_var_3))
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  19 happyReduction_38
happyReduction_38 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn19
		 ((happy_var_1, [])
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happyReduce 4 19 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_1  20 happyReduction_40
happyReduction_40 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn20
		 ((happy_var_1, TVar ("True", []))
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  20 happyReduction_41
happyReduction_41 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn20
		 ((happy_var_1, happy_var_3)
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happyReduce 7 21 happyReduction_42
happyReduction_42 ((HappyAbsSyn20  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((happy_var_2, happy_var_5, happy_var_7)
	) `HappyStk` happyRest

happyReduce_43 = happyReduce 4 21 happyReduction_43
happyReduction_43 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((happy_var_2, [], happy_var_4)
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_0  22 happyReduction_44
happyReduction_44  =  HappyAbsSyn22
		 (RawProgram [] [] []
	)

happyReduce_45 = happySpecReduce_2  22 happyReduction_45
happyReduction_45 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (modify programTypes (++ [happy_var_2]) happy_var_1
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  22 happyReduction_46
happyReduction_46 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (modify programTerms (++ [happy_var_2]) happy_var_1
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  22 happyReduction_47
happyReduction_47 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (modify programProps (++ [happy_var_2]) happy_var_1
	)
happyReduction_47 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 55 55 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 23;
	TokenBar -> cont 24;
	TokenTypeOf -> cont 25;
	TokenArr -> cont 26;
	TokenDArr -> cont 27;
	TokenSet -> cont 28;
	TokenEq -> cont 29;
	TokenOS -> cont 30;
	TokenCS -> cont 31;
	TokenOP -> cont 32;
	TokenCP -> cont 33;
	TokenOA -> cont 34;
	TokenCA -> cont 35;
	TokenComma -> cont 36;
	TokenAbsurd -> cont 37;
	TokenMatch -> cont 38;
	TokenWith -> cont 39;
	TokenFun -> cont 40;
	TokenFix -> cont 41;
	TokenLet -> cont 42;
	TokenIn -> cont 43;
	TokenType -> cont 44;
	TokenEnd -> cont 45;
	TokenInj happy_dollar_dollar -> cont 46;
	TokenInd -> cont 47;
	TokenProp -> cont 48;
	TokenAll -> cont 49;
	TokenIf -> cont 50;
	TokenThen -> cont 51;
	TokenElse -> cont 52;
	TokenFold -> cont 53;
	TokenEqEq -> cont 54;
	_ -> happyError' (tk:tks)
	}

happyError_ 55 tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

happyTerm tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn14 z -> happyReturn z; _other -> notHappyAtAll })

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
