{-# OPTIONS_GHC -w #-}
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

-- parser produced by Happy Version 1.18.10

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
	| HappyAbsSyn21 (TermDef)
	| HappyAbsSyn22 (InstName)
	| HappyAbsSyn23 (PolyName)
	| HappyAbsSyn24 ((RawTerm, RawTerm))
	| HappyAbsSyn25 (PropDef)
	| HappyAbsSyn26 (RawProgram)

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
 action_111 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_47 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (26) = happyGoto action_21
action_0 _ = happyReduce_44

action_1 (27) = happyShift action_6
action_1 (37) = happyShift action_15
action_1 (42) = happyShift action_16
action_1 (44) = happyShift action_17
action_1 (45) = happyShift action_18
action_1 (54) = happyShift action_19
action_1 (59) = happyShift action_20
action_1 (19) = happyGoto action_13
action_1 (22) = happyGoto action_14
action_1 _ = happyFail

action_2 (27) = happyShift action_6
action_2 (37) = happyShift action_12
action_2 (7) = happyGoto action_10
action_2 (22) = happyGoto action_11
action_2 _ = happyFail

action_3 (37) = happyShift action_9
action_3 (17) = happyGoto action_7
action_3 (18) = happyGoto action_8
action_3 _ = happyFail

action_4 (27) = happyShift action_6
action_4 (22) = happyGoto action_5
action_4 _ = happyFail

action_5 _ = happyFail

action_6 (39) = happyShift action_42
action_6 _ = happyReduce_36

action_7 _ = happyReduce_24

action_8 (37) = happyShift action_9
action_8 (60) = happyAccept
action_8 (17) = happyGoto action_41
action_8 _ = happyFail

action_9 (27) = happyShift action_30
action_9 (37) = happyShift action_31
action_9 (10) = happyGoto action_28
action_9 (14) = happyGoto action_40
action_9 _ = happyFail

action_10 (60) = happyAccept
action_10 _ = happyFail

action_11 (30) = happyShift action_39
action_11 _ = happyReduce_4

action_12 (27) = happyShift action_6
action_12 (37) = happyShift action_12
action_12 (7) = happyGoto action_38
action_12 (22) = happyGoto action_11
action_12 _ = happyFail

action_13 (60) = happyAccept
action_13 _ = happyFail

action_14 (20) = happyGoto action_37
action_14 _ = happyReduce_33

action_15 (27) = happyShift action_6
action_15 (37) = happyShift action_15
action_15 (42) = happyShift action_16
action_15 (44) = happyShift action_17
action_15 (45) = happyShift action_18
action_15 (54) = happyShift action_19
action_15 (59) = happyShift action_20
action_15 (19) = happyGoto action_36
action_15 (22) = happyGoto action_14
action_15 _ = happyFail

action_16 (27) = happyShift action_6
action_16 (37) = happyShift action_15
action_16 (42) = happyShift action_16
action_16 (44) = happyShift action_17
action_16 (45) = happyShift action_18
action_16 (54) = happyShift action_19
action_16 (59) = happyShift action_20
action_16 (19) = happyGoto action_35
action_16 (22) = happyGoto action_14
action_16 _ = happyFail

action_17 (37) = happyShift action_9
action_17 (17) = happyGoto action_7
action_17 (18) = happyGoto action_34
action_17 _ = happyFail

action_18 (27) = happyShift action_6
action_18 (37) = happyShift action_12
action_18 (7) = happyGoto action_33
action_18 (22) = happyGoto action_11
action_18 _ = happyFail

action_19 (27) = happyShift action_6
action_19 (37) = happyShift action_15
action_19 (42) = happyShift action_16
action_19 (44) = happyShift action_17
action_19 (45) = happyShift action_18
action_19 (54) = happyShift action_19
action_19 (59) = happyShift action_20
action_19 (19) = happyGoto action_32
action_19 (22) = happyGoto action_14
action_19 _ = happyFail

action_20 (27) = happyShift action_30
action_20 (37) = happyShift action_31
action_20 (10) = happyGoto action_28
action_20 (14) = happyGoto action_29
action_20 _ = happyFail

action_21 (46) = happyShift action_25
action_21 (51) = happyShift action_26
action_21 (52) = happyShift action_27
action_21 (60) = happyAccept
action_21 (9) = happyGoto action_22
action_21 (21) = happyGoto action_23
action_21 (25) = happyGoto action_24
action_21 _ = happyFail

action_22 _ = happyReduce_45

action_23 _ = happyReduce_46

action_24 _ = happyReduce_47

action_25 (27) = happyShift action_58
action_25 (23) = happyGoto action_60
action_25 _ = happyFail

action_26 (27) = happyShift action_58
action_26 (23) = happyGoto action_59
action_26 _ = happyFail

action_27 (27) = happyShift action_58
action_27 (23) = happyGoto action_57
action_27 _ = happyFail

action_28 (27) = happyShift action_56
action_28 _ = happyReduce_18

action_29 (31) = happyShift action_55
action_29 _ = happyFail

action_30 _ = happyReduce_10

action_31 (27) = happyShift action_54
action_31 _ = happyFail

action_32 (55) = happyShift action_53
action_32 _ = happyFail

action_33 (36) = happyShift action_52
action_33 _ = happyFail

action_34 (30) = happyShift action_51
action_34 (37) = happyShift action_9
action_34 (17) = happyGoto action_41
action_34 _ = happyFail

action_35 (43) = happyShift action_50
action_35 _ = happyFail

action_36 (38) = happyShift action_49
action_36 _ = happyFail

action_37 (27) = happyShift action_6
action_37 (37) = happyShift action_15
action_37 (42) = happyShift action_16
action_37 (44) = happyShift action_17
action_37 (45) = happyShift action_18
action_37 (54) = happyShift action_19
action_37 (59) = happyShift action_20
action_37 (19) = happyGoto action_48
action_37 (22) = happyGoto action_14
action_37 _ = happyReduce_26

action_38 (38) = happyShift action_47
action_38 _ = happyFail

action_39 (27) = happyShift action_6
action_39 (37) = happyShift action_12
action_39 (7) = happyGoto action_46
action_39 (22) = happyGoto action_11
action_39 _ = happyFail

action_40 (29) = happyShift action_45
action_40 _ = happyFail

action_41 _ = happyReduce_25

action_42 (27) = happyShift action_6
action_42 (37) = happyShift action_12
action_42 (7) = happyGoto action_43
action_42 (13) = happyGoto action_44
action_42 (22) = happyGoto action_11
action_42 _ = happyFail

action_43 _ = happyReduce_16

action_44 (40) = happyShift action_73
action_44 (41) = happyShift action_74
action_44 _ = happyFail

action_45 (27) = happyShift action_6
action_45 (37) = happyShift action_12
action_45 (7) = happyGoto action_72
action_45 (22) = happyGoto action_11
action_45 _ = happyFail

action_46 _ = happyReduce_5

action_47 (30) = happyShift action_71
action_47 _ = happyFail

action_48 _ = happyReduce_34

action_49 _ = happyReduce_27

action_50 (28) = happyShift action_70
action_50 (15) = happyGoto action_69
action_50 _ = happyFail

action_51 (27) = happyShift action_6
action_51 (37) = happyShift action_15
action_51 (42) = happyShift action_16
action_51 (44) = happyShift action_17
action_51 (45) = happyShift action_18
action_51 (54) = happyShift action_19
action_51 (59) = happyShift action_20
action_51 (19) = happyGoto action_68
action_51 (22) = happyGoto action_14
action_51 _ = happyFail

action_52 _ = happyReduce_29

action_53 (27) = happyShift action_6
action_53 (37) = happyShift action_15
action_53 (42) = happyShift action_16
action_53 (44) = happyShift action_17
action_53 (45) = happyShift action_18
action_53 (54) = happyShift action_19
action_53 (59) = happyShift action_20
action_53 (19) = happyGoto action_67
action_53 (22) = happyGoto action_14
action_53 _ = happyFail

action_54 (41) = happyShift action_66
action_54 _ = happyFail

action_55 (27) = happyShift action_6
action_55 (37) = happyShift action_15
action_55 (42) = happyShift action_16
action_55 (44) = happyShift action_17
action_55 (45) = happyShift action_18
action_55 (54) = happyShift action_19
action_55 (59) = happyShift action_20
action_55 (19) = happyGoto action_65
action_55 (22) = happyGoto action_14
action_55 _ = happyFail

action_56 _ = happyReduce_11

action_57 (29) = happyShift action_64
action_57 _ = happyFail

action_58 (39) = happyShift action_63
action_58 _ = happyReduce_38

action_59 (34) = happyShift action_62
action_59 _ = happyFail

action_60 (37) = happyShift action_9
action_60 (17) = happyGoto action_7
action_60 (18) = happyGoto action_61
action_60 _ = happyFail

action_61 (29) = happyShift action_92
action_61 (37) = happyShift action_9
action_61 (17) = happyGoto action_41
action_61 _ = happyFail

action_62 (27) = happyShift action_30
action_62 (37) = happyShift action_31
action_62 (8) = happyGoto action_90
action_62 (10) = happyGoto action_28
action_62 (14) = happyGoto action_91
action_62 _ = happyFail

action_63 (27) = happyShift action_84
action_63 (11) = happyGoto action_89
action_63 _ = happyFail

action_64 (27) = happyShift action_6
action_64 (37) = happyShift action_15
action_64 (42) = happyShift action_16
action_64 (44) = happyShift action_17
action_64 (45) = happyShift action_18
action_64 (53) = happyShift action_88
action_64 (54) = happyShift action_19
action_64 (59) = happyShift action_20
action_64 (19) = happyGoto action_86
action_64 (22) = happyGoto action_14
action_64 (24) = happyGoto action_87
action_64 _ = happyFail

action_65 (47) = happyShift action_85
action_65 _ = happyFail

action_66 (27) = happyShift action_84
action_66 (11) = happyGoto action_83
action_66 _ = happyFail

action_67 (56) = happyShift action_82
action_67 _ = happyFail

action_68 _ = happyReduce_28

action_69 (28) = happyShift action_80
action_69 (49) = happyShift action_81
action_69 _ = happyFail

action_70 (27) = happyShift action_30
action_70 (37) = happyShift action_31
action_70 (10) = happyGoto action_28
action_70 (14) = happyGoto action_78
action_70 (16) = happyGoto action_79
action_70 _ = happyFail

action_71 (27) = happyShift action_6
action_71 (37) = happyShift action_12
action_71 (7) = happyGoto action_77
action_71 (22) = happyGoto action_11
action_71 _ = happyFail

action_72 (38) = happyShift action_76
action_72 _ = happyFail

action_73 _ = happyReduce_37

action_74 (27) = happyShift action_6
action_74 (37) = happyShift action_12
action_74 (7) = happyGoto action_75
action_74 (22) = happyGoto action_11
action_74 _ = happyFail

action_75 _ = happyReduce_17

action_76 _ = happyReduce_23

action_77 _ = happyReduce_6

action_78 (30) = happyShift action_103
action_78 _ = happyFail

action_79 _ = happyReduce_20

action_80 (27) = happyShift action_30
action_80 (37) = happyShift action_31
action_80 (10) = happyGoto action_28
action_80 (14) = happyGoto action_78
action_80 (16) = happyGoto action_102
action_80 _ = happyFail

action_81 _ = happyReduce_30

action_82 (27) = happyShift action_6
action_82 (37) = happyShift action_15
action_82 (42) = happyShift action_16
action_82 (44) = happyShift action_17
action_82 (45) = happyShift action_18
action_82 (54) = happyShift action_19
action_82 (59) = happyShift action_20
action_82 (19) = happyGoto action_101
action_82 (22) = happyGoto action_14
action_82 _ = happyFail

action_83 (38) = happyShift action_100
action_83 (41) = happyShift action_96
action_83 _ = happyFail

action_84 _ = happyReduce_12

action_85 (27) = happyShift action_6
action_85 (37) = happyShift action_15
action_85 (42) = happyShift action_16
action_85 (44) = happyShift action_17
action_85 (45) = happyShift action_18
action_85 (54) = happyShift action_19
action_85 (59) = happyShift action_20
action_85 (19) = happyGoto action_99
action_85 (22) = happyGoto action_14
action_85 _ = happyFail

action_86 (34) = happyShift action_98
action_86 _ = happyReduce_40

action_87 _ = happyReduce_43

action_88 (37) = happyShift action_9
action_88 (17) = happyGoto action_7
action_88 (18) = happyGoto action_97
action_88 _ = happyFail

action_89 (40) = happyShift action_95
action_89 (41) = happyShift action_96
action_89 _ = happyFail

action_90 (28) = happyShift action_94
action_90 _ = happyReduce_9

action_91 _ = happyReduce_7

action_92 (27) = happyShift action_6
action_92 (37) = happyShift action_12
action_92 (7) = happyGoto action_93
action_92 (22) = happyGoto action_11
action_92 _ = happyFail

action_93 (34) = happyShift action_109
action_93 _ = happyFail

action_94 (27) = happyShift action_30
action_94 (37) = happyShift action_31
action_94 (10) = happyGoto action_28
action_94 (14) = happyGoto action_108
action_94 _ = happyFail

action_95 _ = happyReduce_39

action_96 (27) = happyShift action_107
action_96 _ = happyFail

action_97 (30) = happyShift action_106
action_97 (37) = happyShift action_9
action_97 (17) = happyGoto action_41
action_97 _ = happyFail

action_98 (27) = happyShift action_6
action_98 (37) = happyShift action_15
action_98 (42) = happyShift action_16
action_98 (44) = happyShift action_17
action_98 (45) = happyShift action_18
action_98 (54) = happyShift action_19
action_98 (59) = happyShift action_20
action_98 (19) = happyGoto action_105
action_98 (22) = happyGoto action_14
action_98 _ = happyFail

action_99 _ = happyReduce_31

action_100 _ = happyReduce_19

action_101 _ = happyReduce_32

action_102 _ = happyReduce_21

action_103 (27) = happyShift action_6
action_103 (37) = happyShift action_15
action_103 (42) = happyShift action_16
action_103 (44) = happyShift action_17
action_103 (45) = happyShift action_18
action_103 (54) = happyShift action_19
action_103 (59) = happyShift action_20
action_103 (19) = happyGoto action_104
action_103 (22) = happyGoto action_14
action_103 _ = happyFail

action_104 _ = happyReduce_22

action_105 _ = happyReduce_41

action_106 (27) = happyShift action_6
action_106 (37) = happyShift action_15
action_106 (42) = happyShift action_16
action_106 (44) = happyShift action_17
action_106 (45) = happyShift action_18
action_106 (54) = happyShift action_19
action_106 (59) = happyShift action_20
action_106 (19) = happyGoto action_86
action_106 (22) = happyGoto action_14
action_106 (24) = happyGoto action_111
action_106 _ = happyFail

action_107 _ = happyReduce_13

action_108 _ = happyReduce_8

action_109 (27) = happyShift action_6
action_109 (37) = happyShift action_15
action_109 (42) = happyShift action_16
action_109 (44) = happyShift action_17
action_109 (45) = happyShift action_18
action_109 (54) = happyShift action_19
action_109 (59) = happyShift action_20
action_109 (19) = happyGoto action_110
action_109 (22) = happyGoto action_14
action_109 _ = happyFail

action_110 _ = happyReduce_35

action_111 _ = happyReduce_42

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn7
		 (TyBase happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn7
		 (TyFun (TyBase happy_var_1) happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 5 7 happyReduction_6
happyReduction_6 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TyFun happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 9 happyReduction_9
happyReduction_9 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  10 happyReduction_11
happyReduction_11 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  11 happyReduction_13
happyReduction_13 (HappyTerminal (TokenName happy_var_3))
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  12 happyReduction_15
happyReduction_15 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  13 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  14 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happyReduce 5 14 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ("tuple":happy_var_2:happy_var_4
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_2  15 happyReduction_20
happyReduction_20 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn15
		 ([happy_var_2]
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  15 happyReduction_21
happyReduction_21 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  16 happyReduction_22
happyReduction_22 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn16
		 (TAlt happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 5 17 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (map (\n -> TBind n happy_var_4) happy_var_2
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_1  18 happyReduction_24
happyReduction_24 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  18 happyReduction_25
happyReduction_25 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 ++ happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  19 happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn19
		 (TApp happy_var_1 happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  19 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 4 19 happyReduction_28
happyReduction_28 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_3  19 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (TUnr happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happyReduce 5 19 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TCase happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 6 19 happyReduction_31
happyReduction_31 ((HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TAssert happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 6 19 happyReduction_32
happyReduction_32 ((HappyAbsSyn19  happy_var_6) `HappyStk`
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

happyReduce_33 = happySpecReduce_0  20 happyReduction_33
happyReduction_33  =  HappyAbsSyn12
		 ([]
	)

happyReduce_34 = happySpecReduce_2  20 happyReduction_34
happyReduction_34 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happyReduce 7 21 happyReduction_35
happyReduction_35 ((HappyAbsSyn19  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((happy_var_2, happy_var_3, happy_var_5, happy_var_7)
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_1  22 happyReduction_36
happyReduction_36 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn22
		 ((happy_var_1, [])
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happyReduce 4 22 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_1  23 happyReduction_38
happyReduction_38 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn23
		 ((happy_var_1, [])
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happyReduce 4 23 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_1  24 happyReduction_40
happyReduction_40 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn24
		 ((TApp ("True", []) [], happy_var_1)
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  24 happyReduction_41
happyReduction_41 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn24
		 ((happy_var_1, happy_var_3)
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happyReduce 7 25 happyReduction_42
happyReduction_42 ((HappyAbsSyn24  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 ((happy_var_2, happy_var_5, happy_var_7)
	) `HappyStk` happyRest

happyReduce_43 = happyReduce 4 25 happyReduction_43
happyReduction_43 ((HappyAbsSyn24  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 ((happy_var_2, [], happy_var_4)
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_0  26 happyReduction_44
happyReduction_44  =  HappyAbsSyn26
		 (RawProgram [] [] []
	)

happyReduce_45 = happySpecReduce_2  26 happyReduction_45
happyReduction_45 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (modify programTypes (++ [happy_var_2]) happy_var_1
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  26 happyReduction_46
happyReduction_46 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (modify programTerms (++ [happy_var_2]) happy_var_1
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  26 happyReduction_47
happyReduction_47 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (modify programProps (++ [happy_var_2]) happy_var_1
	)
happyReduction_47 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 60 60 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 27;
	TokenBar -> cont 28;
	TokenTypeOf -> cont 29;
	TokenRArr -> cont 30;
	TokenLArr -> cont 31;
	TokenDRArr -> cont 32;
	TokenSet -> cont 33;
	TokenEq -> cont 34;
	TokenOS -> cont 35;
	TokenCS -> cont 36;
	TokenOP -> cont 37;
	TokenCP -> cont 38;
	TokenOA -> cont 39;
	TokenCA -> cont 40;
	TokenComma -> cont 41;
	TokenMatch -> cont 42;
	TokenWith -> cont 43;
	TokenFun -> cont 44;
	TokenUnr -> cont 45;
	TokenLet -> cont 46;
	TokenIn -> cont 47;
	TokenType -> cont 48;
	TokenEnd -> cont 49;
	TokenInj happy_dollar_dollar -> cont 50;
	TokenInd -> cont 51;
	TokenProp -> cont 52;
	TokenAll -> cont 53;
	TokenIf -> cont 54;
	TokenThen -> cont 55;
	TokenElse -> cont 56;
	TokenFold -> cont 57;
	TokenEqEq -> cont 58;
	TokenAssert -> cont 59;
	_ -> happyError' (tk:tks)
	}

happyError_ 60 tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn26 z -> happyReturn z; _other -> notHappyAtAll })

happyTerm tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

happyType tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

happyBindings tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
