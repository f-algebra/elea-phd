{-# OPTIONS_GHC -w #-}
module Elea.Parser 
(
  program, term, ty
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

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn6 (RawBind)
	| HappyAbsSyn7 ([RawBind])
	| HappyAbsSyn8 (RawType)
	| HappyAbsSyn9 ([String])
	| HappyAbsSyn10 ([RawAlt])
	| HappyAbsSyn11 (RawAlt)
	| HappyAbsSyn13 (RawTerm)
	| HappyAbsSyn14 (TypeDef)
	| HappyAbsSyn15 (TermDef)
	| HappyAbsSyn16 (RawProgram)

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
 action_95 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_36 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (31) = happyShift action_21
action_0 (33) = happyShift action_22
action_0 (14) = happyGoto action_18
action_0 (15) = happyGoto action_19
action_0 (16) = happyGoto action_20
action_0 _ = happyFail

action_1 (17) = happyShift action_11
action_1 (25) = happyShift action_12
action_1 (27) = happyShift action_13
action_1 (29) = happyShift action_14
action_1 (30) = happyShift action_15
action_1 (31) = happyShift action_16
action_1 (35) = happyShift action_17
action_1 (13) = happyGoto action_10
action_1 _ = happyFail

action_2 (17) = happyShift action_6
action_2 (21) = happyShift action_7
action_2 (25) = happyShift action_8
action_2 (30) = happyShift action_9
action_2 (8) = happyGoto action_5
action_2 _ = happyFail

action_3 (17) = happyShift action_4
action_3 _ = happyFail

action_4 (19) = happyShift action_44
action_4 _ = happyFail

action_5 (17) = happyShift action_41
action_5 (20) = happyShift action_42
action_5 (25) = happyShift action_43
action_5 (36) = happyAccept
action_5 _ = happyFail

action_6 _ = happyReduce_6

action_7 _ = happyReduce_7

action_8 (17) = happyShift action_40
action_8 (21) = happyShift action_7
action_8 (25) = happyShift action_8
action_8 (30) = happyShift action_9
action_8 (6) = happyGoto action_38
action_8 (8) = happyGoto action_39
action_8 _ = happyFail

action_9 (17) = happyShift action_37
action_9 _ = happyFail

action_10 (17) = happyShift action_34
action_10 (23) = happyShift action_35
action_10 (25) = happyShift action_36
action_10 (36) = happyAccept
action_10 _ = happyFail

action_11 _ = happyReduce_21

action_12 (17) = happyShift action_11
action_12 (25) = happyShift action_12
action_12 (27) = happyShift action_13
action_12 (29) = happyShift action_14
action_12 (30) = happyShift action_15
action_12 (31) = happyShift action_16
action_12 (35) = happyShift action_17
action_12 (13) = happyGoto action_33
action_12 _ = happyFail

action_13 (17) = happyShift action_11
action_13 (25) = happyShift action_12
action_13 (27) = happyShift action_13
action_13 (29) = happyShift action_14
action_13 (30) = happyShift action_15
action_13 (31) = happyShift action_16
action_13 (35) = happyShift action_17
action_13 (13) = happyGoto action_32
action_13 _ = happyFail

action_14 (25) = happyShift action_30
action_14 (12) = happyGoto action_31
action_14 _ = happyFail

action_15 (25) = happyShift action_30
action_15 (12) = happyGoto action_29
action_15 _ = happyFail

action_16 (17) = happyShift action_28
action_16 _ = happyFail

action_17 (23) = happyShift action_27
action_17 _ = happyFail

action_18 _ = happyReduce_33

action_19 _ = happyReduce_34

action_20 (31) = happyShift action_21
action_20 (33) = happyShift action_22
action_20 (36) = happyAccept
action_20 (14) = happyGoto action_25
action_20 (15) = happyGoto action_26
action_20 _ = happyFail

action_21 (17) = happyShift action_24
action_21 _ = happyFail

action_22 (17) = happyShift action_23
action_22 _ = happyFail

action_23 (22) = happyShift action_62
action_23 _ = happyFail

action_24 (22) = happyShift action_61
action_24 _ = happyFail

action_25 _ = happyReduce_35

action_26 _ = happyReduce_36

action_27 (17) = happyShift action_6
action_27 (21) = happyShift action_7
action_27 (25) = happyShift action_8
action_27 (30) = happyShift action_9
action_27 (8) = happyGoto action_60
action_27 _ = happyFail

action_28 (22) = happyShift action_59
action_28 _ = happyFail

action_29 (20) = happyShift action_58
action_29 (25) = happyShift action_56
action_29 _ = happyFail

action_30 (17) = happyShift action_4
action_30 (6) = happyGoto action_57
action_30 _ = happyFail

action_31 (20) = happyShift action_55
action_31 (25) = happyShift action_56
action_31 _ = happyFail

action_32 (17) = happyShift action_34
action_32 (23) = happyShift action_35
action_32 (25) = happyShift action_36
action_32 (28) = happyShift action_54
action_32 _ = happyFail

action_33 (17) = happyShift action_34
action_33 (23) = happyShift action_35
action_33 (25) = happyShift action_36
action_33 (26) = happyShift action_53
action_33 _ = happyFail

action_34 _ = happyReduce_23

action_35 (17) = happyShift action_6
action_35 (21) = happyShift action_7
action_35 (25) = happyShift action_8
action_35 (30) = happyShift action_9
action_35 (8) = happyGoto action_52
action_35 _ = happyFail

action_36 (17) = happyShift action_11
action_36 (25) = happyShift action_12
action_36 (27) = happyShift action_13
action_36 (29) = happyShift action_14
action_36 (30) = happyShift action_15
action_36 (31) = happyShift action_16
action_36 (35) = happyShift action_17
action_36 (13) = happyGoto action_51
action_36 _ = happyFail

action_37 (28) = happyShift action_50
action_37 _ = happyFail

action_38 (26) = happyShift action_49
action_38 _ = happyFail

action_39 (17) = happyShift action_41
action_39 (20) = happyShift action_42
action_39 (25) = happyShift action_43
action_39 (26) = happyShift action_48
action_39 _ = happyFail

action_40 (19) = happyShift action_44
action_40 _ = happyReduce_6

action_41 _ = happyReduce_8

action_42 (17) = happyShift action_6
action_42 (21) = happyShift action_7
action_42 (25) = happyShift action_8
action_42 (30) = happyShift action_9
action_42 (8) = happyGoto action_47
action_42 _ = happyFail

action_43 (17) = happyShift action_6
action_43 (21) = happyShift action_7
action_43 (25) = happyShift action_8
action_43 (30) = happyShift action_9
action_43 (8) = happyGoto action_46
action_43 _ = happyFail

action_44 (17) = happyShift action_6
action_44 (21) = happyShift action_7
action_44 (25) = happyShift action_8
action_44 (30) = happyShift action_9
action_44 (8) = happyGoto action_45
action_44 _ = happyFail

action_45 (17) = happyShift action_41
action_45 (20) = happyShift action_42
action_45 (25) = happyShift action_43
action_45 _ = happyReduce_3

action_46 (17) = happyShift action_41
action_46 (20) = happyShift action_42
action_46 (25) = happyShift action_43
action_46 (26) = happyShift action_78
action_46 _ = happyFail

action_47 (17) = happyShift action_41
action_47 (20) = happyShift action_42
action_47 (25) = happyShift action_43
action_47 _ = happyReduce_10

action_48 _ = happyReduce_12

action_49 (20) = happyShift action_77
action_49 _ = happyFail

action_50 (18) = happyShift action_76
action_50 (7) = happyGoto action_75
action_50 _ = happyFail

action_51 (17) = happyShift action_34
action_51 (23) = happyShift action_35
action_51 (25) = happyShift action_36
action_51 (26) = happyShift action_74
action_51 _ = happyFail

action_52 (17) = happyShift action_41
action_52 (20) = happyShift action_42
action_52 (24) = happyShift action_73
action_52 (25) = happyShift action_43
action_52 _ = happyFail

action_53 _ = happyReduce_26

action_54 (18) = happyShift action_72
action_54 (10) = happyGoto action_71
action_54 _ = happyFail

action_55 (17) = happyShift action_11
action_55 (25) = happyShift action_12
action_55 (27) = happyShift action_13
action_55 (29) = happyShift action_14
action_55 (30) = happyShift action_15
action_55 (31) = happyShift action_16
action_55 (35) = happyShift action_17
action_55 (13) = happyGoto action_70
action_55 _ = happyFail

action_56 (17) = happyShift action_4
action_56 (6) = happyGoto action_69
action_56 _ = happyFail

action_57 (26) = happyShift action_68
action_57 _ = happyFail

action_58 (17) = happyShift action_11
action_58 (25) = happyShift action_12
action_58 (27) = happyShift action_13
action_58 (29) = happyShift action_14
action_58 (30) = happyShift action_15
action_58 (31) = happyShift action_16
action_58 (35) = happyShift action_17
action_58 (13) = happyGoto action_67
action_58 _ = happyFail

action_59 (17) = happyShift action_11
action_59 (25) = happyShift action_12
action_59 (27) = happyShift action_13
action_59 (29) = happyShift action_14
action_59 (30) = happyShift action_15
action_59 (31) = happyShift action_16
action_59 (35) = happyShift action_17
action_59 (13) = happyGoto action_66
action_59 _ = happyFail

action_60 (17) = happyShift action_41
action_60 (20) = happyShift action_42
action_60 (24) = happyShift action_65
action_60 (25) = happyShift action_43
action_60 _ = happyFail

action_61 (17) = happyShift action_11
action_61 (25) = happyShift action_12
action_61 (27) = happyShift action_13
action_61 (29) = happyShift action_14
action_61 (30) = happyShift action_15
action_61 (31) = happyShift action_16
action_61 (35) = happyShift action_17
action_61 (13) = happyGoto action_64
action_61 _ = happyFail

action_62 (17) = happyShift action_6
action_62 (21) = happyShift action_7
action_62 (25) = happyShift action_8
action_62 (30) = happyShift action_9
action_62 (8) = happyGoto action_63
action_62 _ = happyFail

action_63 (17) = happyShift action_41
action_63 (20) = happyShift action_42
action_63 (25) = happyShift action_43
action_63 _ = happyReduce_31

action_64 (17) = happyShift action_34
action_64 (23) = happyShift action_35
action_64 (25) = happyShift action_36
action_64 _ = happyReduce_32

action_65 _ = happyReduce_22

action_66 (17) = happyShift action_34
action_66 (23) = happyShift action_35
action_66 (25) = happyShift action_36
action_66 (32) = happyShift action_89
action_66 _ = happyFail

action_67 (17) = happyShift action_34
action_67 (23) = happyShift action_35
action_67 (25) = happyShift action_36
action_67 _ = happyReduce_28

action_68 _ = happyReduce_19

action_69 (26) = happyShift action_88
action_69 _ = happyFail

action_70 (17) = happyShift action_34
action_70 (23) = happyShift action_35
action_70 (25) = happyShift action_36
action_70 _ = happyReduce_27

action_71 (18) = happyShift action_86
action_71 (34) = happyShift action_87
action_71 _ = happyFail

action_72 (17) = happyShift action_85
action_72 (9) = happyGoto action_83
action_72 (11) = happyGoto action_84
action_72 _ = happyFail

action_73 _ = happyReduce_25

action_74 _ = happyReduce_24

action_75 (18) = happyShift action_81
action_75 (34) = happyShift action_82
action_75 _ = happyFail

action_76 (17) = happyShift action_4
action_76 (6) = happyGoto action_80
action_76 _ = happyFail

action_77 (17) = happyShift action_6
action_77 (21) = happyShift action_7
action_77 (25) = happyShift action_8
action_77 (30) = happyShift action_9
action_77 (8) = happyGoto action_79
action_77 _ = happyFail

action_78 _ = happyReduce_9

action_79 (17) = happyShift action_41
action_79 (20) = happyShift action_42
action_79 (25) = happyShift action_43
action_79 _ = happyReduce_11

action_80 _ = happyReduce_4

action_81 (17) = happyShift action_4
action_81 (6) = happyGoto action_94
action_81 _ = happyFail

action_82 _ = happyReduce_13

action_83 (17) = happyShift action_92
action_83 (20) = happyShift action_93
action_83 _ = happyFail

action_84 _ = happyReduce_16

action_85 _ = happyReduce_14

action_86 (17) = happyShift action_85
action_86 (9) = happyGoto action_83
action_86 (11) = happyGoto action_91
action_86 _ = happyFail

action_87 _ = happyReduce_29

action_88 _ = happyReduce_20

action_89 (17) = happyShift action_11
action_89 (25) = happyShift action_12
action_89 (27) = happyShift action_13
action_89 (29) = happyShift action_14
action_89 (30) = happyShift action_15
action_89 (31) = happyShift action_16
action_89 (35) = happyShift action_17
action_89 (13) = happyGoto action_90
action_89 _ = happyFail

action_90 (17) = happyShift action_34
action_90 (23) = happyShift action_35
action_90 (25) = happyShift action_36
action_90 _ = happyReduce_30

action_91 _ = happyReduce_17

action_92 _ = happyReduce_15

action_93 (17) = happyShift action_11
action_93 (25) = happyShift action_12
action_93 (27) = happyShift action_13
action_93 (29) = happyShift action_14
action_93 (30) = happyShift action_15
action_93 (31) = happyShift action_16
action_93 (35) = happyShift action_17
action_93 (13) = happyGoto action_95
action_93 _ = happyFail

action_94 _ = happyReduce_5

action_95 (17) = happyShift action_34
action_95 (23) = happyShift action_35
action_95 (25) = happyShift action_36
action_95 _ = happyReduce_18

happyReduce_3 = happySpecReduce_3  6 happyReduction_3
happyReduction_3 (HappyAbsSyn8  happy_var_3)
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn6
		 (TBind (Just happy_var_1) happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  7 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn7
		 ([happy_var_2]
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  8 happyReduction_6
happyReduction_6 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn8
		 (TyVar happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn8
		 (TySet
	)

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (TyApp happy_var_1 (TyVar happy_var_2)
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 8 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TyApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_3  8 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (TyFun (TBind Nothing happy_var_1) happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 5 8 happyReduction_11
happyReduction_11 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TyFun happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 5 8 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TyInd (TBind (Just happy_var_2) TySet) happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  9 happyReduction_15
happyReduction_15 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  10 happyReduction_16
happyReduction_16 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 ([happy_var_2]
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  10 happyReduction_17
happyReduction_17 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn11
		 (TAlt happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn7
		 ([happy_var_2]
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 12 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (happy_var_1 ++ [happy_var_3]
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  13 happyReduction_21
happyReduction_21 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn13
		 (TVar happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happyReduce 4 13 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInj happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TInj happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_2  13 happyReduction_23
happyReduction_23 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (TApp happy_var_1 (TVar happy_var_2)
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 4 13 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 4 13 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TApp happy_var_1 (TType happy_var_3)
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_3  13 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 13 happyReduction_27
happyReduction_27 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 4 13 happyReduction_28
happyReduction_28 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TFix happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 5 13 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TCase happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 6 13 happyReduction_30
happyReduction_30 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TLet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 4 14 happyReduction_31
happyReduction_31 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 4 15 happyReduction_32
happyReduction_32 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_1  16 happyReduction_33
happyReduction_33 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn16
		 (Program [] [happy_var_1]
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  16 happyReduction_34
happyReduction_34 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (Program [happy_var_1] []
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  16 happyReduction_35
happyReduction_35 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (modify programTypes (++ [happy_var_2]) happy_var_1
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  16 happyReduction_36
happyReduction_36 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (modify programTerms (++ [happy_var_2]) happy_var_1
	)
happyReduction_36 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 36 36 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 17;
	TokenBar -> cont 18;
	TokenTypeOf -> cont 19;
	TokenArr -> cont 20;
	TokenSet -> cont 21;
	TokenEq -> cont 22;
	TokenOS -> cont 23;
	TokenCS -> cont 24;
	TokenOP -> cont 25;
	TokenCP -> cont 26;
	TokenMatch -> cont 27;
	TokenWith -> cont 28;
	TokenFun -> cont 29;
	TokenFix -> cont 30;
	TokenLet -> cont 31;
	TokenIn -> cont 32;
	TokenType -> cont 33;
	TokenEnd -> cont 34;
	TokenInj happy_dollar_dollar -> cont 35;
	_ -> happyError' (tk:tks)
	}

happyError_ 36 tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn16 z -> happyReturn z; _other -> notHappyAtAll })

happyTerm tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

happyType tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
term = withEmptyScope 
     . parseRawTerm 
     . happyTerm 
     . lexer
  
ty :: (Err.Monad m, Defs.Monad m) => String -> m Type
ty = withEmptyScope
   . parseRawType
   . happyType
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
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<inbyggd>" #-}
{-# LINE 1 "<kommandorad>" #-}
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
