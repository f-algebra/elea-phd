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
import qualified Elea.Term.Constraint as Constraint
import qualified Elea.Term.Index as Indices
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Ext as Term
import qualified Elea.Type.Ext as Type
import qualified Elea.Monad.Env as Env
import qualified Elea.Foldable as Fold
import qualified Elea.Transform.Simplify as Simp
import qualified Elea.Transform.Evaluate as Eval
import qualified Elea.Monad.Definitions.Class as Defs      
import qualified Elea.Monad.Error.Class as Err
import qualified Data.Map as Map
import Control.Applicative(Applicative(..))

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
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (24) = happyGoto action_25
action_0 _ = happyReduce_55

action_1 (25) = happyShift action_6
action_1 (38) = happyShift action_14
action_1 (43) = happyShift action_15
action_1 (44) = happyShift action_16
action_1 (46) = happyShift action_17
action_1 (47) = happyShift action_18
action_1 (48) = happyShift action_19
action_1 (56) = happyShift action_20
action_1 (59) = happyShift action_21
action_1 (60) = happyShift action_22
action_1 (61) = happyShift action_23
action_1 (62) = happyShift action_24
action_1 (19) = happyGoto action_12
action_1 (21) = happyGoto action_13
action_1 _ = happyFail

action_2 (25) = happyShift action_6
action_2 (38) = happyShift action_11
action_2 (7) = happyGoto action_10
action_2 (21) = happyGoto action_5
action_2 _ = happyFail

action_3 (38) = happyShift action_9
action_3 (17) = happyGoto action_7
action_3 (18) = happyGoto action_8
action_3 _ = happyFail

action_4 (25) = happyShift action_6
action_4 (21) = happyGoto action_5
action_4 _ = happyFail

action_5 _ = happyReduce_4

action_6 (40) = happyShift action_55
action_6 _ = happyReduce_47

action_7 _ = happyReduce_25

action_8 (38) = happyShift action_9
action_8 (63) = happyAccept
action_8 (17) = happyGoto action_54
action_8 _ = happyFail

action_9 (25) = happyShift action_35
action_9 (38) = happyShift action_36
action_9 (10) = happyGoto action_33
action_9 (14) = happyGoto action_53
action_9 _ = happyFail

action_10 (28) = happyShift action_52
action_10 (63) = happyAccept
action_10 _ = happyFail

action_11 (25) = happyShift action_6
action_11 (38) = happyShift action_11
action_11 (7) = happyGoto action_51
action_11 (21) = happyGoto action_5
action_11 _ = happyFail

action_12 (25) = happyShift action_6
action_12 (31) = happyShift action_47
action_12 (32) = happyShift action_48
action_12 (33) = happyShift action_49
action_12 (38) = happyShift action_50
action_12 (63) = happyAccept
action_12 (21) = happyGoto action_46
action_12 _ = happyFail

action_13 _ = happyReduce_27

action_14 (25) = happyShift action_6
action_14 (38) = happyShift action_14
action_14 (43) = happyShift action_15
action_14 (44) = happyShift action_16
action_14 (46) = happyShift action_17
action_14 (47) = happyShift action_18
action_14 (48) = happyShift action_19
action_14 (56) = happyShift action_20
action_14 (59) = happyShift action_21
action_14 (60) = happyShift action_22
action_14 (61) = happyShift action_23
action_14 (62) = happyShift action_24
action_14 (19) = happyGoto action_45
action_14 (21) = happyGoto action_13
action_14 _ = happyFail

action_15 (25) = happyShift action_6
action_15 (38) = happyShift action_11
action_15 (7) = happyGoto action_44
action_15 (21) = happyGoto action_5
action_15 _ = happyFail

action_16 (25) = happyShift action_6
action_16 (38) = happyShift action_14
action_16 (43) = happyShift action_15
action_16 (44) = happyShift action_16
action_16 (46) = happyShift action_17
action_16 (47) = happyShift action_18
action_16 (48) = happyShift action_19
action_16 (56) = happyShift action_20
action_16 (59) = happyShift action_21
action_16 (60) = happyShift action_22
action_16 (61) = happyShift action_23
action_16 (62) = happyShift action_24
action_16 (19) = happyGoto action_43
action_16 (21) = happyGoto action_13
action_16 _ = happyFail

action_17 (38) = happyShift action_9
action_17 (17) = happyGoto action_7
action_17 (18) = happyGoto action_42
action_17 _ = happyFail

action_18 (38) = happyShift action_9
action_18 (17) = happyGoto action_7
action_18 (18) = happyGoto action_41
action_18 _ = happyFail

action_19 (25) = happyShift action_40
action_19 _ = happyFail

action_20 (25) = happyShift action_6
action_20 (38) = happyShift action_14
action_20 (43) = happyShift action_15
action_20 (44) = happyShift action_16
action_20 (46) = happyShift action_17
action_20 (47) = happyShift action_18
action_20 (48) = happyShift action_19
action_20 (56) = happyShift action_20
action_20 (59) = happyShift action_21
action_20 (60) = happyShift action_22
action_20 (61) = happyShift action_23
action_20 (62) = happyShift action_24
action_20 (19) = happyGoto action_39
action_20 (21) = happyGoto action_13
action_20 _ = happyFail

action_21 (25) = happyShift action_6
action_21 (38) = happyShift action_14
action_21 (43) = happyShift action_15
action_21 (44) = happyShift action_16
action_21 (46) = happyShift action_17
action_21 (47) = happyShift action_18
action_21 (48) = happyShift action_19
action_21 (56) = happyShift action_20
action_21 (59) = happyShift action_21
action_21 (60) = happyShift action_22
action_21 (61) = happyShift action_23
action_21 (62) = happyShift action_24
action_21 (19) = happyGoto action_38
action_21 (21) = happyGoto action_13
action_21 _ = happyFail

action_22 (25) = happyShift action_6
action_22 (38) = happyShift action_11
action_22 (7) = happyGoto action_37
action_22 (21) = happyGoto action_5
action_22 _ = happyFail

action_23 (25) = happyShift action_35
action_23 (38) = happyShift action_36
action_23 (10) = happyGoto action_33
action_23 (14) = happyGoto action_34
action_23 _ = happyFail

action_24 (25) = happyShift action_6
action_24 (38) = happyShift action_14
action_24 (43) = happyShift action_15
action_24 (44) = happyShift action_16
action_24 (46) = happyShift action_17
action_24 (47) = happyShift action_18
action_24 (48) = happyShift action_19
action_24 (56) = happyShift action_20
action_24 (59) = happyShift action_21
action_24 (60) = happyShift action_22
action_24 (61) = happyShift action_23
action_24 (62) = happyShift action_24
action_24 (19) = happyGoto action_32
action_24 (21) = happyGoto action_13
action_24 _ = happyFail

action_25 (48) = happyShift action_29
action_25 (53) = happyShift action_30
action_25 (54) = happyShift action_31
action_25 (63) = happyAccept
action_25 (9) = happyGoto action_26
action_25 (20) = happyGoto action_27
action_25 (23) = happyGoto action_28
action_25 _ = happyFail

action_26 _ = happyReduce_56

action_27 _ = happyReduce_57

action_28 _ = happyReduce_58

action_29 (25) = happyShift action_81
action_29 (22) = happyGoto action_84
action_29 _ = happyFail

action_30 (25) = happyShift action_81
action_30 (22) = happyGoto action_83
action_30 _ = happyFail

action_31 (25) = happyShift action_81
action_31 (38) = happyShift action_9
action_31 (40) = happyShift action_82
action_31 (17) = happyGoto action_7
action_31 (18) = happyGoto action_79
action_31 (22) = happyGoto action_80
action_31 _ = happyFail

action_32 (25) = happyShift action_6
action_32 (31) = happyShift action_47
action_32 (32) = happyShift action_48
action_32 (33) = happyShift action_49
action_32 (38) = happyShift action_50
action_32 (49) = happyShift action_78
action_32 (21) = happyGoto action_46
action_32 _ = happyFail

action_33 (25) = happyShift action_77
action_33 _ = happyReduce_19

action_34 (29) = happyShift action_76
action_34 _ = happyFail

action_35 _ = happyReduce_11

action_36 (25) = happyShift action_75
action_36 _ = happyFail

action_37 (28) = happyShift action_52
action_37 (37) = happyShift action_74
action_37 _ = happyFail

action_38 (25) = happyShift action_6
action_38 (31) = happyShift action_47
action_38 (32) = happyShift action_48
action_38 (33) = happyShift action_49
action_38 (38) = happyShift action_50
action_38 (49) = happyShift action_73
action_38 (21) = happyGoto action_46
action_38 _ = happyFail

action_39 (25) = happyShift action_6
action_39 (31) = happyShift action_47
action_39 (32) = happyShift action_48
action_39 (33) = happyShift action_49
action_39 (38) = happyShift action_50
action_39 (57) = happyShift action_72
action_39 (21) = happyGoto action_46
action_39 _ = happyFail

action_40 (35) = happyShift action_71
action_40 _ = happyFail

action_41 (28) = happyShift action_70
action_41 (38) = happyShift action_9
action_41 (17) = happyGoto action_54
action_41 _ = happyFail

action_42 (28) = happyShift action_69
action_42 (38) = happyShift action_9
action_42 (17) = happyGoto action_54
action_42 _ = happyFail

action_43 (25) = happyShift action_6
action_43 (31) = happyShift action_47
action_43 (32) = happyShift action_48
action_43 (33) = happyShift action_49
action_43 (38) = happyShift action_50
action_43 (45) = happyShift action_68
action_43 (21) = happyGoto action_46
action_43 _ = happyFail

action_44 (28) = happyShift action_52
action_44 _ = happyReduce_28

action_45 (25) = happyShift action_6
action_45 (31) = happyShift action_47
action_45 (32) = happyShift action_48
action_45 (33) = happyShift action_49
action_45 (38) = happyShift action_50
action_45 (39) = happyShift action_66
action_45 (42) = happyShift action_67
action_45 (21) = happyGoto action_46
action_45 _ = happyFail

action_46 _ = happyReduce_29

action_47 (25) = happyShift action_6
action_47 (38) = happyShift action_14
action_47 (43) = happyShift action_15
action_47 (44) = happyShift action_16
action_47 (46) = happyShift action_17
action_47 (47) = happyShift action_18
action_47 (48) = happyShift action_19
action_47 (56) = happyShift action_20
action_47 (59) = happyShift action_21
action_47 (60) = happyShift action_22
action_47 (61) = happyShift action_23
action_47 (62) = happyShift action_24
action_47 (19) = happyGoto action_65
action_47 (21) = happyGoto action_13
action_47 _ = happyFail

action_48 (25) = happyShift action_6
action_48 (38) = happyShift action_14
action_48 (43) = happyShift action_15
action_48 (44) = happyShift action_16
action_48 (46) = happyShift action_17
action_48 (47) = happyShift action_18
action_48 (48) = happyShift action_19
action_48 (56) = happyShift action_20
action_48 (59) = happyShift action_21
action_48 (60) = happyShift action_22
action_48 (61) = happyShift action_23
action_48 (62) = happyShift action_24
action_48 (19) = happyGoto action_64
action_48 (21) = happyGoto action_13
action_48 _ = happyFail

action_49 (25) = happyShift action_6
action_49 (38) = happyShift action_14
action_49 (43) = happyShift action_15
action_49 (44) = happyShift action_16
action_49 (46) = happyShift action_17
action_49 (47) = happyShift action_18
action_49 (48) = happyShift action_19
action_49 (56) = happyShift action_20
action_49 (59) = happyShift action_21
action_49 (60) = happyShift action_22
action_49 (61) = happyShift action_23
action_49 (62) = happyShift action_24
action_49 (19) = happyGoto action_63
action_49 (21) = happyGoto action_13
action_49 _ = happyFail

action_50 (25) = happyShift action_6
action_50 (38) = happyShift action_14
action_50 (43) = happyShift action_15
action_50 (44) = happyShift action_16
action_50 (46) = happyShift action_17
action_50 (47) = happyShift action_18
action_50 (48) = happyShift action_19
action_50 (56) = happyShift action_20
action_50 (59) = happyShift action_21
action_50 (60) = happyShift action_22
action_50 (61) = happyShift action_23
action_50 (62) = happyShift action_24
action_50 (19) = happyGoto action_62
action_50 (21) = happyGoto action_13
action_50 _ = happyFail

action_51 (28) = happyShift action_52
action_51 (39) = happyShift action_60
action_51 (42) = happyShift action_61
action_51 _ = happyFail

action_52 (25) = happyShift action_6
action_52 (38) = happyShift action_11
action_52 (7) = happyGoto action_59
action_52 (21) = happyGoto action_5
action_52 _ = happyFail

action_53 (27) = happyShift action_58
action_53 _ = happyFail

action_54 _ = happyReduce_26

action_55 (25) = happyShift action_6
action_55 (38) = happyShift action_11
action_55 (7) = happyGoto action_56
action_55 (13) = happyGoto action_57
action_55 (21) = happyGoto action_5
action_55 _ = happyFail

action_56 (28) = happyShift action_52
action_56 _ = happyReduce_17

action_57 (41) = happyShift action_109
action_57 (42) = happyShift action_110
action_57 _ = happyFail

action_58 (25) = happyShift action_6
action_58 (38) = happyShift action_11
action_58 (7) = happyGoto action_108
action_58 (21) = happyGoto action_5
action_58 _ = happyFail

action_59 (28) = happyShift action_52
action_59 _ = happyReduce_7

action_60 _ = happyReduce_5

action_61 (25) = happyShift action_6
action_61 (38) = happyShift action_11
action_61 (7) = happyGoto action_56
action_61 (13) = happyGoto action_107
action_61 (21) = happyGoto action_5
action_61 _ = happyFail

action_62 (25) = happyShift action_6
action_62 (31) = happyShift action_47
action_62 (32) = happyShift action_48
action_62 (33) = happyShift action_49
action_62 (38) = happyShift action_50
action_62 (39) = happyShift action_105
action_62 (42) = happyShift action_106
action_62 (21) = happyGoto action_46
action_62 _ = happyFail

action_63 (25) = happyShift action_6
action_63 (31) = happyShift action_47
action_63 (32) = happyShift action_48
action_63 (33) = happyShift action_49
action_63 (38) = happyShift action_50
action_63 (21) = happyGoto action_46
action_63 _ = happyReduce_39

action_64 (25) = happyShift action_6
action_64 (31) = happyShift action_47
action_64 (32) = happyShift action_48
action_64 (33) = happyShift action_49
action_64 (38) = happyShift action_50
action_64 (21) = happyGoto action_46
action_64 _ = happyReduce_38

action_65 (25) = happyShift action_6
action_65 (31) = happyShift action_47
action_65 (32) = happyShift action_48
action_65 (33) = happyShift action_49
action_65 (38) = happyShift action_50
action_65 (21) = happyGoto action_46
action_65 _ = happyReduce_37

action_66 _ = happyReduce_31

action_67 (25) = happyShift action_6
action_67 (38) = happyShift action_14
action_67 (43) = happyShift action_15
action_67 (44) = happyShift action_16
action_67 (46) = happyShift action_17
action_67 (47) = happyShift action_18
action_67 (48) = happyShift action_19
action_67 (56) = happyShift action_20
action_67 (59) = happyShift action_21
action_67 (60) = happyShift action_22
action_67 (61) = happyShift action_23
action_67 (62) = happyShift action_24
action_67 (12) = happyGoto action_103
action_67 (19) = happyGoto action_104
action_67 (21) = happyGoto action_13
action_67 _ = happyFail

action_68 (26) = happyShift action_102
action_68 (15) = happyGoto action_101
action_68 _ = happyFail

action_69 (25) = happyShift action_6
action_69 (38) = happyShift action_14
action_69 (43) = happyShift action_15
action_69 (44) = happyShift action_16
action_69 (46) = happyShift action_17
action_69 (47) = happyShift action_18
action_69 (48) = happyShift action_19
action_69 (56) = happyShift action_20
action_69 (59) = happyShift action_21
action_69 (60) = happyShift action_22
action_69 (61) = happyShift action_23
action_69 (62) = happyShift action_24
action_69 (19) = happyGoto action_100
action_69 (21) = happyGoto action_13
action_69 _ = happyFail

action_70 (25) = happyShift action_6
action_70 (38) = happyShift action_14
action_70 (43) = happyShift action_15
action_70 (44) = happyShift action_16
action_70 (46) = happyShift action_17
action_70 (47) = happyShift action_18
action_70 (48) = happyShift action_19
action_70 (56) = happyShift action_20
action_70 (59) = happyShift action_21
action_70 (60) = happyShift action_22
action_70 (61) = happyShift action_23
action_70 (62) = happyShift action_24
action_70 (19) = happyGoto action_99
action_70 (21) = happyGoto action_13
action_70 _ = happyFail

action_71 (25) = happyShift action_6
action_71 (38) = happyShift action_14
action_71 (43) = happyShift action_15
action_71 (44) = happyShift action_16
action_71 (46) = happyShift action_17
action_71 (47) = happyShift action_18
action_71 (48) = happyShift action_19
action_71 (56) = happyShift action_20
action_71 (59) = happyShift action_21
action_71 (60) = happyShift action_22
action_71 (61) = happyShift action_23
action_71 (62) = happyShift action_24
action_71 (19) = happyGoto action_98
action_71 (21) = happyGoto action_13
action_71 _ = happyFail

action_72 (25) = happyShift action_6
action_72 (38) = happyShift action_14
action_72 (43) = happyShift action_15
action_72 (44) = happyShift action_16
action_72 (46) = happyShift action_17
action_72 (47) = happyShift action_18
action_72 (48) = happyShift action_19
action_72 (56) = happyShift action_20
action_72 (59) = happyShift action_21
action_72 (60) = happyShift action_22
action_72 (61) = happyShift action_23
action_72 (62) = happyShift action_24
action_72 (19) = happyGoto action_97
action_72 (21) = happyGoto action_13
action_72 _ = happyFail

action_73 (25) = happyShift action_6
action_73 (38) = happyShift action_14
action_73 (43) = happyShift action_15
action_73 (44) = happyShift action_16
action_73 (46) = happyShift action_17
action_73 (47) = happyShift action_18
action_73 (48) = happyShift action_19
action_73 (56) = happyShift action_20
action_73 (59) = happyShift action_21
action_73 (60) = happyShift action_22
action_73 (61) = happyShift action_23
action_73 (62) = happyShift action_24
action_73 (19) = happyGoto action_96
action_73 (21) = happyGoto action_13
action_73 _ = happyFail

action_74 _ = happyReduce_41

action_75 (42) = happyShift action_95
action_75 _ = happyFail

action_76 (25) = happyShift action_6
action_76 (38) = happyShift action_14
action_76 (43) = happyShift action_15
action_76 (44) = happyShift action_16
action_76 (46) = happyShift action_17
action_76 (47) = happyShift action_18
action_76 (48) = happyShift action_19
action_76 (56) = happyShift action_20
action_76 (59) = happyShift action_21
action_76 (60) = happyShift action_22
action_76 (61) = happyShift action_23
action_76 (62) = happyShift action_24
action_76 (19) = happyGoto action_94
action_76 (21) = happyGoto action_13
action_76 _ = happyFail

action_77 _ = happyReduce_12

action_78 (25) = happyShift action_6
action_78 (38) = happyShift action_14
action_78 (43) = happyShift action_15
action_78 (44) = happyShift action_16
action_78 (46) = happyShift action_17
action_78 (47) = happyShift action_18
action_78 (48) = happyShift action_19
action_78 (56) = happyShift action_20
action_78 (59) = happyShift action_21
action_78 (60) = happyShift action_22
action_78 (61) = happyShift action_23
action_78 (62) = happyShift action_24
action_78 (19) = happyGoto action_93
action_78 (21) = happyGoto action_13
action_78 _ = happyFail

action_79 (28) = happyShift action_92
action_79 (38) = happyShift action_9
action_79 (17) = happyGoto action_54
action_79 _ = happyFail

action_80 (28) = happyShift action_91
action_80 (38) = happyShift action_9
action_80 (17) = happyGoto action_7
action_80 (18) = happyGoto action_90
action_80 _ = happyFail

action_81 (40) = happyShift action_89
action_81 _ = happyReduce_49

action_82 (25) = happyShift action_88
action_82 (11) = happyGoto action_87
action_82 _ = happyFail

action_83 (35) = happyShift action_86
action_83 _ = happyFail

action_84 (35) = happyShift action_85
action_84 _ = happyFail

action_85 (25) = happyShift action_6
action_85 (38) = happyShift action_14
action_85 (43) = happyShift action_15
action_85 (44) = happyShift action_16
action_85 (46) = happyShift action_17
action_85 (47) = happyShift action_18
action_85 (48) = happyShift action_19
action_85 (56) = happyShift action_20
action_85 (59) = happyShift action_21
action_85 (60) = happyShift action_22
action_85 (61) = happyShift action_23
action_85 (62) = happyShift action_24
action_85 (19) = happyGoto action_133
action_85 (21) = happyGoto action_13
action_85 _ = happyFail

action_86 (25) = happyShift action_35
action_86 (38) = happyShift action_36
action_86 (8) = happyGoto action_131
action_86 (10) = happyGoto action_33
action_86 (14) = happyGoto action_132
action_86 _ = happyFail

action_87 (41) = happyShift action_129
action_87 (42) = happyShift action_130
action_87 _ = happyFail

action_88 _ = happyReduce_13

action_89 (25) = happyShift action_88
action_89 (11) = happyGoto action_128
action_89 _ = happyFail

action_90 (28) = happyShift action_127
action_90 (38) = happyShift action_9
action_90 (17) = happyGoto action_54
action_90 _ = happyFail

action_91 (25) = happyShift action_6
action_91 (38) = happyShift action_14
action_91 (43) = happyShift action_15
action_91 (44) = happyShift action_16
action_91 (46) = happyShift action_17
action_91 (47) = happyShift action_18
action_91 (48) = happyShift action_19
action_91 (56) = happyShift action_20
action_91 (59) = happyShift action_21
action_91 (60) = happyShift action_22
action_91 (61) = happyShift action_23
action_91 (62) = happyShift action_24
action_91 (19) = happyGoto action_126
action_91 (21) = happyGoto action_13
action_91 _ = happyFail

action_92 (25) = happyShift action_6
action_92 (38) = happyShift action_14
action_92 (43) = happyShift action_15
action_92 (44) = happyShift action_16
action_92 (46) = happyShift action_17
action_92 (47) = happyShift action_18
action_92 (48) = happyShift action_19
action_92 (56) = happyShift action_20
action_92 (59) = happyShift action_21
action_92 (60) = happyShift action_22
action_92 (61) = happyShift action_23
action_92 (62) = happyShift action_24
action_92 (19) = happyGoto action_125
action_92 (21) = happyGoto action_13
action_92 _ = happyFail

action_93 (25) = happyShift action_6
action_93 (31) = happyShift action_47
action_93 (32) = happyShift action_48
action_93 (33) = happyShift action_49
action_93 (38) = happyShift action_50
action_93 (21) = happyGoto action_46
action_93 _ = happyReduce_44

action_94 (25) = happyShift action_6
action_94 (31) = happyShift action_47
action_94 (32) = happyShift action_48
action_94 (33) = happyShift action_49
action_94 (38) = happyShift action_50
action_94 (49) = happyShift action_124
action_94 (21) = happyGoto action_46
action_94 _ = happyFail

action_95 (25) = happyShift action_88
action_95 (11) = happyGoto action_123
action_95 _ = happyFail

action_96 (25) = happyShift action_6
action_96 (31) = happyShift action_47
action_96 (32) = happyShift action_48
action_96 (33) = happyShift action_49
action_96 (38) = happyShift action_50
action_96 (21) = happyGoto action_46
action_96 _ = happyReduce_36

action_97 (25) = happyShift action_6
action_97 (31) = happyShift action_47
action_97 (32) = happyShift action_48
action_97 (33) = happyShift action_49
action_97 (38) = happyShift action_50
action_97 (58) = happyShift action_122
action_97 (21) = happyGoto action_46
action_97 _ = happyFail

action_98 (25) = happyShift action_6
action_98 (31) = happyShift action_47
action_98 (32) = happyShift action_48
action_98 (33) = happyShift action_49
action_98 (38) = happyShift action_50
action_98 (49) = happyShift action_121
action_98 (21) = happyGoto action_46
action_98 _ = happyFail

action_99 (25) = happyShift action_6
action_99 (31) = happyShift action_47
action_99 (32) = happyShift action_48
action_99 (33) = happyShift action_49
action_99 (38) = happyShift action_50
action_99 (21) = happyGoto action_46
action_99 _ = happyReduce_35

action_100 (25) = happyShift action_6
action_100 (31) = happyShift action_47
action_100 (32) = happyShift action_48
action_100 (33) = happyShift action_49
action_100 (38) = happyShift action_50
action_100 (21) = happyGoto action_46
action_100 _ = happyReduce_34

action_101 (26) = happyShift action_119
action_101 (51) = happyShift action_120
action_101 _ = happyFail

action_102 (25) = happyShift action_35
action_102 (38) = happyShift action_36
action_102 (10) = happyGoto action_33
action_102 (14) = happyGoto action_117
action_102 (16) = happyGoto action_118
action_102 _ = happyFail

action_103 (39) = happyShift action_115
action_103 (42) = happyShift action_116
action_103 _ = happyFail

action_104 (25) = happyShift action_6
action_104 (31) = happyShift action_47
action_104 (32) = happyShift action_48
action_104 (33) = happyShift action_49
action_104 (38) = happyShift action_50
action_104 (21) = happyGoto action_46
action_104 _ = happyReduce_15

action_105 _ = happyReduce_30

action_106 (25) = happyShift action_6
action_106 (38) = happyShift action_14
action_106 (43) = happyShift action_15
action_106 (44) = happyShift action_16
action_106 (46) = happyShift action_17
action_106 (47) = happyShift action_18
action_106 (48) = happyShift action_19
action_106 (56) = happyShift action_20
action_106 (59) = happyShift action_21
action_106 (60) = happyShift action_22
action_106 (61) = happyShift action_23
action_106 (62) = happyShift action_24
action_106 (12) = happyGoto action_114
action_106 (19) = happyGoto action_104
action_106 (21) = happyGoto action_13
action_106 _ = happyFail

action_107 (39) = happyShift action_113
action_107 (42) = happyShift action_110
action_107 _ = happyFail

action_108 (28) = happyShift action_52
action_108 (39) = happyShift action_112
action_108 _ = happyFail

action_109 _ = happyReduce_48

action_110 (25) = happyShift action_6
action_110 (38) = happyShift action_11
action_110 (7) = happyGoto action_111
action_110 (21) = happyGoto action_5
action_110 _ = happyFail

action_111 (28) = happyShift action_52
action_111 _ = happyReduce_18

action_112 _ = happyReduce_24

action_113 _ = happyReduce_6

action_114 (39) = happyShift action_146
action_114 (42) = happyShift action_116
action_114 _ = happyFail

action_115 _ = happyReduce_33

action_116 (25) = happyShift action_6
action_116 (38) = happyShift action_14
action_116 (43) = happyShift action_15
action_116 (44) = happyShift action_16
action_116 (46) = happyShift action_17
action_116 (47) = happyShift action_18
action_116 (48) = happyShift action_19
action_116 (56) = happyShift action_20
action_116 (59) = happyShift action_21
action_116 (60) = happyShift action_22
action_116 (61) = happyShift action_23
action_116 (62) = happyShift action_24
action_116 (19) = happyGoto action_145
action_116 (21) = happyGoto action_13
action_116 _ = happyFail

action_117 (28) = happyShift action_144
action_117 _ = happyFail

action_118 _ = happyReduce_21

action_119 (25) = happyShift action_35
action_119 (38) = happyShift action_36
action_119 (10) = happyGoto action_33
action_119 (14) = happyGoto action_117
action_119 (16) = happyGoto action_143
action_119 _ = happyFail

action_120 _ = happyReduce_42

action_121 (25) = happyShift action_6
action_121 (38) = happyShift action_14
action_121 (43) = happyShift action_15
action_121 (44) = happyShift action_16
action_121 (46) = happyShift action_17
action_121 (47) = happyShift action_18
action_121 (48) = happyShift action_19
action_121 (56) = happyShift action_20
action_121 (59) = happyShift action_21
action_121 (60) = happyShift action_22
action_121 (61) = happyShift action_23
action_121 (62) = happyShift action_24
action_121 (19) = happyGoto action_142
action_121 (21) = happyGoto action_13
action_121 _ = happyFail

action_122 (25) = happyShift action_6
action_122 (38) = happyShift action_14
action_122 (43) = happyShift action_15
action_122 (44) = happyShift action_16
action_122 (46) = happyShift action_17
action_122 (47) = happyShift action_18
action_122 (48) = happyShift action_19
action_122 (56) = happyShift action_20
action_122 (59) = happyShift action_21
action_122 (60) = happyShift action_22
action_122 (61) = happyShift action_23
action_122 (62) = happyShift action_24
action_122 (19) = happyGoto action_141
action_122 (21) = happyGoto action_13
action_122 _ = happyFail

action_123 (39) = happyShift action_140
action_123 (42) = happyShift action_130
action_123 _ = happyFail

action_124 (25) = happyShift action_6
action_124 (38) = happyShift action_14
action_124 (43) = happyShift action_15
action_124 (44) = happyShift action_16
action_124 (46) = happyShift action_17
action_124 (47) = happyShift action_18
action_124 (48) = happyShift action_19
action_124 (56) = happyShift action_20
action_124 (59) = happyShift action_21
action_124 (60) = happyShift action_22
action_124 (61) = happyShift action_23
action_124 (62) = happyShift action_24
action_124 (19) = happyGoto action_139
action_124 (21) = happyGoto action_13
action_124 _ = happyFail

action_125 (25) = happyShift action_6
action_125 (31) = happyShift action_47
action_125 (32) = happyShift action_48
action_125 (33) = happyShift action_49
action_125 (38) = happyShift action_50
action_125 (21) = happyGoto action_46
action_125 _ = happyReduce_54

action_126 (25) = happyShift action_6
action_126 (31) = happyShift action_47
action_126 (32) = happyShift action_48
action_126 (33) = happyShift action_49
action_126 (38) = happyShift action_50
action_126 (21) = happyGoto action_46
action_126 _ = happyReduce_52

action_127 (25) = happyShift action_6
action_127 (38) = happyShift action_14
action_127 (43) = happyShift action_15
action_127 (44) = happyShift action_16
action_127 (46) = happyShift action_17
action_127 (47) = happyShift action_18
action_127 (48) = happyShift action_19
action_127 (56) = happyShift action_20
action_127 (59) = happyShift action_21
action_127 (60) = happyShift action_22
action_127 (61) = happyShift action_23
action_127 (62) = happyShift action_24
action_127 (19) = happyGoto action_138
action_127 (21) = happyGoto action_13
action_127 _ = happyFail

action_128 (41) = happyShift action_137
action_128 (42) = happyShift action_130
action_128 _ = happyFail

action_129 (38) = happyShift action_9
action_129 (17) = happyGoto action_7
action_129 (18) = happyGoto action_136
action_129 _ = happyFail

action_130 (25) = happyShift action_135
action_130 _ = happyFail

action_131 (26) = happyShift action_134
action_131 _ = happyReduce_10

action_132 _ = happyReduce_8

action_133 (25) = happyShift action_6
action_133 (31) = happyShift action_47
action_133 (32) = happyShift action_48
action_133 (33) = happyShift action_49
action_133 (38) = happyShift action_50
action_133 (21) = happyGoto action_46
action_133 _ = happyReduce_46

action_134 (25) = happyShift action_35
action_134 (38) = happyShift action_36
action_134 (10) = happyGoto action_33
action_134 (14) = happyGoto action_149
action_134 _ = happyFail

action_135 _ = happyReduce_14

action_136 (28) = happyShift action_148
action_136 (38) = happyShift action_9
action_136 (17) = happyGoto action_54
action_136 _ = happyFail

action_137 _ = happyReduce_50

action_138 (25) = happyShift action_6
action_138 (31) = happyShift action_47
action_138 (32) = happyShift action_48
action_138 (33) = happyShift action_49
action_138 (38) = happyShift action_50
action_138 (21) = happyGoto action_46
action_138 _ = happyReduce_51

action_139 (25) = happyShift action_6
action_139 (31) = happyShift action_47
action_139 (32) = happyShift action_48
action_139 (33) = happyShift action_49
action_139 (38) = happyShift action_50
action_139 (21) = happyGoto action_46
action_139 _ = happyReduce_43

action_140 _ = happyReduce_20

action_141 (25) = happyShift action_6
action_141 (31) = happyShift action_47
action_141 (32) = happyShift action_48
action_141 (33) = happyShift action_49
action_141 (38) = happyShift action_50
action_141 (21) = happyGoto action_46
action_141 _ = happyReduce_45

action_142 (25) = happyShift action_6
action_142 (31) = happyShift action_47
action_142 (32) = happyShift action_48
action_142 (33) = happyShift action_49
action_142 (38) = happyShift action_50
action_142 (21) = happyGoto action_46
action_142 _ = happyReduce_40

action_143 _ = happyReduce_22

action_144 (25) = happyShift action_6
action_144 (38) = happyShift action_14
action_144 (43) = happyShift action_15
action_144 (44) = happyShift action_16
action_144 (46) = happyShift action_17
action_144 (47) = happyShift action_18
action_144 (48) = happyShift action_19
action_144 (56) = happyShift action_20
action_144 (59) = happyShift action_21
action_144 (60) = happyShift action_22
action_144 (61) = happyShift action_23
action_144 (62) = happyShift action_24
action_144 (19) = happyGoto action_147
action_144 (21) = happyGoto action_13
action_144 _ = happyFail

action_145 (25) = happyShift action_6
action_145 (31) = happyShift action_47
action_145 (32) = happyShift action_48
action_145 (33) = happyShift action_49
action_145 (38) = happyShift action_50
action_145 (21) = happyGoto action_46
action_145 _ = happyReduce_16

action_146 _ = happyReduce_32

action_147 (25) = happyShift action_6
action_147 (31) = happyShift action_47
action_147 (32) = happyShift action_48
action_147 (33) = happyShift action_49
action_147 (38) = happyShift action_50
action_147 (21) = happyGoto action_46
action_147 _ = happyReduce_23

action_148 (25) = happyShift action_6
action_148 (38) = happyShift action_14
action_148 (43) = happyShift action_15
action_148 (44) = happyShift action_16
action_148 (46) = happyShift action_17
action_148 (47) = happyShift action_18
action_148 (48) = happyShift action_19
action_148 (56) = happyShift action_20
action_148 (59) = happyShift action_21
action_148 (60) = happyShift action_22
action_148 (61) = happyShift action_23
action_148 (62) = happyShift action_24
action_148 (19) = happyGoto action_150
action_148 (21) = happyGoto action_13
action_148 _ = happyFail

action_149 _ = happyReduce_9

action_150 (25) = happyShift action_6
action_150 (31) = happyShift action_47
action_150 (32) = happyShift action_48
action_150 (33) = happyShift action_49
action_150 (38) = happyShift action_50
action_150 (21) = happyGoto action_46
action_150 _ = happyReduce_53

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

happyReduce_36 = happyReduce 4 19 happyReduction_36
happyReduction_36 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TSeq happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_3  19 happyReduction_37
happyReduction_37 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (TLeq happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  19 happyReduction_38
happyReduction_38 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (TLeq happy_var_3 happy_var_1
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  19 happyReduction_39
happyReduction_39 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (TEq happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happyReduce 6 19 happyReduction_40
happyReduction_40 ((HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TLet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_3  19 happyReduction_41
happyReduction_41 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (TFold happy_var_2
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happyReduce 5 19 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TCase happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_43 = happyReduce 6 19 happyReduction_43
happyReduction_43 ((HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TAssert happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_44 = happyReduce 4 19 happyReduction_44
happyReduction_44 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TAssert [] happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_45 = happyReduce 6 19 happyReduction_45
happyReduction_45 ((HappyAbsSyn19  happy_var_6) `HappyStk`
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

happyReduce_46 = happyReduce 4 20 happyReduction_46
happyReduction_46 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_1  21 happyReduction_47
happyReduction_47 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn21
		 ((happy_var_1, [])
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happyReduce 4 21 happyReduction_48
happyReduction_48 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_49 = happySpecReduce_1  22 happyReduction_49
happyReduction_49 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn22
		 ((happy_var_1, [])
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happyReduce 4 22 happyReduction_50
happyReduction_50 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_51 = happyReduce 5 23 happyReduction_51
happyReduction_51 ((HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((happy_var_2, happy_var_3, happy_var_5)
	) `HappyStk` happyRest

happyReduce_52 = happyReduce 4 23 happyReduction_52
happyReduction_52 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((happy_var_2, [], happy_var_4)
	) `HappyStk` happyRest

happyReduce_53 = happyReduce 7 23 happyReduction_53
happyReduction_53 ((HappyAbsSyn19  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((("", happy_var_3), happy_var_5, happy_var_7)
	) `HappyStk` happyRest

happyReduce_54 = happyReduce 4 23 happyReduction_54
happyReduction_54 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((("", []), happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_55 = happySpecReduce_0  24 happyReduction_55
happyReduction_55  =  HappyAbsSyn24
		 (RawProgram [] [] []
	)

happyReduce_56 = happySpecReduce_2  24 happyReduction_56
happyReduction_56 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (modify programTypes (++ [happy_var_2]) happy_var_1
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_2  24 happyReduction_57
happyReduction_57 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (modify programTerms (++ [happy_var_2]) happy_var_1
	)
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  24 happyReduction_58
happyReduction_58 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (modify programProps (++ [happy_var_2]) happy_var_1
	)
happyReduction_58 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 63 63 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 25;
	TokenBar -> cont 26;
	TokenTypeOf -> cont 27;
	TokenRArr -> cont 28;
	TokenLArr -> cont 29;
	TokenDRArr -> cont 30;
	TokenLeq -> cont 31;
	TokenGeq -> cont 32;
	TokenEqEq -> cont 33;
	TokenSet -> cont 34;
	TokenEq -> cont 35;
	TokenOS -> cont 36;
	TokenCS -> cont 37;
	TokenOP -> cont 38;
	TokenCP -> cont 39;
	TokenOA -> cont 40;
	TokenCA -> cont 41;
	TokenComma -> cont 42;
	TokenUnr -> cont 43;
	TokenMatch -> cont 44;
	TokenWith -> cont 45;
	TokenFun -> cont 46;
	TokenFix -> cont 47;
	TokenLet -> cont 48;
	TokenIn -> cont 49;
	TokenType -> cont 50;
	TokenEnd -> cont 51;
	TokenInj happy_dollar_dollar -> cont 52;
	TokenInd -> cont 53;
	TokenProp -> cont 54;
	TokenAll -> cont 55;
	TokenIf -> cont 56;
	TokenThen -> cont 57;
	TokenElse -> cont 58;
	TokenSeq -> cont 59;
	TokenFold -> cont 60;
	TokenAssert -> cont 61;
	TokenAssBool -> cont 62;
	_ -> happyError' (tk:tks)
	}

happyError_ 63 tk tks = happyError' tks
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
    addToMap = Map.insert (get bindLabel b) (Var at)
    
  matched _ = id
  forgetMatches _ = id
  
instance Err.Throws m => Env.Read (ReaderT Scope m) where
  bindings = asks (get bindStack)
  
type ParserMonad m a = 
  ParserEnv m => ReaderT Scope m a
  
type ParserEnv m = 
  (Err.Throws m, Defs.Has m, Tag.Gen m)

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
  
term :: (ParserEnv m, Env.Read m) => String -> m Term
term str = do
  bs <- Env.bindings
  withEmptyScope
    . liftM Eval.run
    . Env.bindMany (reverse bs)
    . parseAndCheckTerm 
    . happyTerm 
    . lexer
    $ str
  
_type :: ParserEnv m => String -> m Type
_type = id
  . withEmptyScope
  . parseRawType
  . happyType 
  . lexer
  
bindings :: ParserEnv m => String -> m [Bind]
bindings = id
  . withEmptyScope
  . mapM parseRawBind
  . happyBindings
  . lexer
  
program :: forall m . ParserEnv m 
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
    Defs.defineTerm name (fmap Simp.run p_term)
    
lookupTerm :: ParamCall -> ParserMonad m Term
lookupTerm (name, raw_ty_args) = do
  mby_bind <- asks (Map.lookup name . get bindMap)
  if length raw_ty_args == 0 && isJust mby_bind
  then return (fromJust mby_bind)
  else do
    ty_args <- mapM parseRawType raw_ty_args
    mby_term <- Defs.lookupTerm name ty_args
    case mby_term of
      Nothing -> Err.throw ("Undefined term: " ++ name)
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
    else Err.throw $ "Undefined inductive type: " ++ name
    
parseAndCheckTerm :: RawTerm -> ParserMonad m Term
parseAndCheckTerm rt = do
  t <- parseRawTerm rt
  valid <- Type.validM t
  if valid
  then return t
  else do
    ts <- showM t
    error ("parsed term has invalid type: " ++ ts)
  
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
      Err.throw 
        $ "Pattern matching over non inductive type [" ++ show ind_ty ++ "]"
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
