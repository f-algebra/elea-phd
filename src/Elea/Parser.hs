{-# OPTIONS_GHC -w #-}
module Elea.Parser 
(
  program, term
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
	| HappyAbsSyn5 (RawBind)
	| HappyAbsSyn6 ([RawBind])
	| HappyAbsSyn7 (RawType)
	| HappyAbsSyn8 ([String])
	| HappyAbsSyn9 ([RawAlt])
	| HappyAbsSyn10 (RawAlt)
	| HappyAbsSyn12 (RawTerm)
	| HappyAbsSyn13 (TypeDef)
	| HappyAbsSyn14 (TermDef)
	| HappyAbsSyn15 (RawProgram)

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
 action_93 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_2,
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
 happyReduce_35 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (30) = happyShift action_15
action_0 (32) = happyShift action_16
action_0 (13) = happyGoto action_12
action_0 (14) = happyGoto action_13
action_0 (15) = happyGoto action_14
action_0 _ = happyFail

action_1 (16) = happyShift action_5
action_1 (24) = happyShift action_6
action_1 (26) = happyShift action_7
action_1 (28) = happyShift action_8
action_1 (29) = happyShift action_9
action_1 (30) = happyShift action_10
action_1 (34) = happyShift action_11
action_1 (12) = happyGoto action_4
action_1 _ = happyFail

action_2 (16) = happyShift action_3
action_2 _ = happyFail

action_3 (18) = happyShift action_31
action_3 _ = happyFail

action_4 (16) = happyShift action_28
action_4 (22) = happyShift action_29
action_4 (24) = happyShift action_30
action_4 (35) = happyAccept
action_4 _ = happyFail

action_5 _ = happyReduce_20

action_6 (16) = happyShift action_5
action_6 (24) = happyShift action_6
action_6 (26) = happyShift action_7
action_6 (28) = happyShift action_8
action_6 (29) = happyShift action_9
action_6 (30) = happyShift action_10
action_6 (34) = happyShift action_11
action_6 (12) = happyGoto action_27
action_6 _ = happyFail

action_7 (16) = happyShift action_5
action_7 (24) = happyShift action_6
action_7 (26) = happyShift action_7
action_7 (28) = happyShift action_8
action_7 (29) = happyShift action_9
action_7 (30) = happyShift action_10
action_7 (34) = happyShift action_11
action_7 (12) = happyGoto action_26
action_7 _ = happyFail

action_8 (24) = happyShift action_24
action_8 (11) = happyGoto action_25
action_8 _ = happyFail

action_9 (24) = happyShift action_24
action_9 (11) = happyGoto action_23
action_9 _ = happyFail

action_10 (16) = happyShift action_22
action_10 _ = happyFail

action_11 (22) = happyShift action_21
action_11 _ = happyFail

action_12 _ = happyReduce_32

action_13 _ = happyReduce_33

action_14 (30) = happyShift action_15
action_14 (32) = happyShift action_16
action_14 (35) = happyAccept
action_14 (13) = happyGoto action_19
action_14 (14) = happyGoto action_20
action_14 _ = happyFail

action_15 (16) = happyShift action_18
action_15 _ = happyFail

action_16 (16) = happyShift action_17
action_16 _ = happyFail

action_17 (21) = happyShift action_48
action_17 _ = happyFail

action_18 (21) = happyShift action_47
action_18 _ = happyFail

action_19 _ = happyReduce_34

action_20 _ = happyReduce_35

action_21 (16) = happyShift action_33
action_21 (20) = happyShift action_34
action_21 (24) = happyShift action_35
action_21 (29) = happyShift action_36
action_21 (7) = happyGoto action_46
action_21 _ = happyFail

action_22 (21) = happyShift action_45
action_22 _ = happyFail

action_23 (19) = happyShift action_44
action_23 (24) = happyShift action_42
action_23 _ = happyFail

action_24 (16) = happyShift action_3
action_24 (5) = happyGoto action_43
action_24 _ = happyFail

action_25 (19) = happyShift action_41
action_25 (24) = happyShift action_42
action_25 _ = happyFail

action_26 (16) = happyShift action_28
action_26 (22) = happyShift action_29
action_26 (24) = happyShift action_30
action_26 (27) = happyShift action_40
action_26 _ = happyFail

action_27 (16) = happyShift action_28
action_27 (22) = happyShift action_29
action_27 (24) = happyShift action_30
action_27 (25) = happyShift action_39
action_27 _ = happyFail

action_28 _ = happyReduce_22

action_29 (16) = happyShift action_33
action_29 (20) = happyShift action_34
action_29 (24) = happyShift action_35
action_29 (29) = happyShift action_36
action_29 (7) = happyGoto action_38
action_29 _ = happyFail

action_30 (16) = happyShift action_5
action_30 (24) = happyShift action_6
action_30 (26) = happyShift action_7
action_30 (28) = happyShift action_8
action_30 (29) = happyShift action_9
action_30 (30) = happyShift action_10
action_30 (34) = happyShift action_11
action_30 (12) = happyGoto action_37
action_30 _ = happyFail

action_31 (16) = happyShift action_33
action_31 (20) = happyShift action_34
action_31 (24) = happyShift action_35
action_31 (29) = happyShift action_36
action_31 (7) = happyGoto action_32
action_31 _ = happyFail

action_32 (16) = happyShift action_51
action_32 (19) = happyShift action_52
action_32 (24) = happyShift action_54
action_32 _ = happyReduce_2

action_33 _ = happyReduce_5

action_34 _ = happyReduce_6

action_35 (16) = happyShift action_67
action_35 (20) = happyShift action_34
action_35 (24) = happyShift action_35
action_35 (29) = happyShift action_36
action_35 (5) = happyGoto action_65
action_35 (7) = happyGoto action_66
action_35 _ = happyFail

action_36 (16) = happyShift action_64
action_36 _ = happyFail

action_37 (16) = happyShift action_28
action_37 (22) = happyShift action_29
action_37 (24) = happyShift action_30
action_37 (25) = happyShift action_63
action_37 _ = happyFail

action_38 (16) = happyShift action_51
action_38 (19) = happyShift action_52
action_38 (23) = happyShift action_62
action_38 (24) = happyShift action_54
action_38 _ = happyFail

action_39 _ = happyReduce_25

action_40 (17) = happyShift action_61
action_40 (9) = happyGoto action_60
action_40 _ = happyFail

action_41 (16) = happyShift action_5
action_41 (24) = happyShift action_6
action_41 (26) = happyShift action_7
action_41 (28) = happyShift action_8
action_41 (29) = happyShift action_9
action_41 (30) = happyShift action_10
action_41 (34) = happyShift action_11
action_41 (12) = happyGoto action_59
action_41 _ = happyFail

action_42 (16) = happyShift action_3
action_42 (5) = happyGoto action_58
action_42 _ = happyFail

action_43 (25) = happyShift action_57
action_43 _ = happyFail

action_44 (16) = happyShift action_5
action_44 (24) = happyShift action_6
action_44 (26) = happyShift action_7
action_44 (28) = happyShift action_8
action_44 (29) = happyShift action_9
action_44 (30) = happyShift action_10
action_44 (34) = happyShift action_11
action_44 (12) = happyGoto action_56
action_44 _ = happyFail

action_45 (16) = happyShift action_5
action_45 (24) = happyShift action_6
action_45 (26) = happyShift action_7
action_45 (28) = happyShift action_8
action_45 (29) = happyShift action_9
action_45 (30) = happyShift action_10
action_45 (34) = happyShift action_11
action_45 (12) = happyGoto action_55
action_45 _ = happyFail

action_46 (16) = happyShift action_51
action_46 (19) = happyShift action_52
action_46 (23) = happyShift action_53
action_46 (24) = happyShift action_54
action_46 _ = happyFail

action_47 (16) = happyShift action_5
action_47 (24) = happyShift action_6
action_47 (26) = happyShift action_7
action_47 (28) = happyShift action_8
action_47 (29) = happyShift action_9
action_47 (30) = happyShift action_10
action_47 (34) = happyShift action_11
action_47 (12) = happyGoto action_50
action_47 _ = happyFail

action_48 (16) = happyShift action_33
action_48 (20) = happyShift action_34
action_48 (24) = happyShift action_35
action_48 (29) = happyShift action_36
action_48 (7) = happyGoto action_49
action_48 _ = happyFail

action_49 (16) = happyShift action_51
action_49 (19) = happyShift action_52
action_49 (24) = happyShift action_54
action_49 _ = happyReduce_30

action_50 (16) = happyShift action_28
action_50 (22) = happyShift action_29
action_50 (24) = happyShift action_30
action_50 _ = happyReduce_31

action_51 _ = happyReduce_7

action_52 (16) = happyShift action_33
action_52 (20) = happyShift action_34
action_52 (24) = happyShift action_35
action_52 (29) = happyShift action_36
action_52 (7) = happyGoto action_79
action_52 _ = happyFail

action_53 _ = happyReduce_21

action_54 (16) = happyShift action_33
action_54 (20) = happyShift action_34
action_54 (24) = happyShift action_35
action_54 (29) = happyShift action_36
action_54 (7) = happyGoto action_78
action_54 _ = happyFail

action_55 (16) = happyShift action_28
action_55 (22) = happyShift action_29
action_55 (24) = happyShift action_30
action_55 (31) = happyShift action_77
action_55 _ = happyFail

action_56 (16) = happyShift action_28
action_56 (22) = happyShift action_29
action_56 (24) = happyShift action_30
action_56 _ = happyReduce_27

action_57 _ = happyReduce_18

action_58 (25) = happyShift action_76
action_58 _ = happyFail

action_59 (16) = happyShift action_28
action_59 (22) = happyShift action_29
action_59 (24) = happyShift action_30
action_59 _ = happyReduce_26

action_60 (17) = happyShift action_74
action_60 (33) = happyShift action_75
action_60 _ = happyFail

action_61 (16) = happyShift action_73
action_61 (8) = happyGoto action_71
action_61 (10) = happyGoto action_72
action_61 _ = happyFail

action_62 _ = happyReduce_24

action_63 _ = happyReduce_23

action_64 (27) = happyShift action_70
action_64 _ = happyFail

action_65 (25) = happyShift action_69
action_65 _ = happyFail

action_66 (16) = happyShift action_51
action_66 (19) = happyShift action_52
action_66 (24) = happyShift action_54
action_66 (25) = happyShift action_68
action_66 _ = happyFail

action_67 (18) = happyShift action_31
action_67 _ = happyReduce_5

action_68 _ = happyReduce_11

action_69 (19) = happyShift action_87
action_69 _ = happyFail

action_70 (17) = happyShift action_86
action_70 (6) = happyGoto action_85
action_70 _ = happyFail

action_71 (16) = happyShift action_83
action_71 (19) = happyShift action_84
action_71 _ = happyFail

action_72 _ = happyReduce_15

action_73 _ = happyReduce_13

action_74 (16) = happyShift action_73
action_74 (8) = happyGoto action_71
action_74 (10) = happyGoto action_82
action_74 _ = happyFail

action_75 _ = happyReduce_28

action_76 _ = happyReduce_19

action_77 (16) = happyShift action_5
action_77 (24) = happyShift action_6
action_77 (26) = happyShift action_7
action_77 (28) = happyShift action_8
action_77 (29) = happyShift action_9
action_77 (30) = happyShift action_10
action_77 (34) = happyShift action_11
action_77 (12) = happyGoto action_81
action_77 _ = happyFail

action_78 (16) = happyShift action_51
action_78 (19) = happyShift action_52
action_78 (24) = happyShift action_54
action_78 (25) = happyShift action_80
action_78 _ = happyFail

action_79 (16) = happyShift action_51
action_79 (19) = happyShift action_52
action_79 (24) = happyShift action_54
action_79 _ = happyReduce_9

action_80 _ = happyReduce_8

action_81 (16) = happyShift action_28
action_81 (22) = happyShift action_29
action_81 (24) = happyShift action_30
action_81 _ = happyReduce_29

action_82 _ = happyReduce_16

action_83 _ = happyReduce_14

action_84 (16) = happyShift action_5
action_84 (24) = happyShift action_6
action_84 (26) = happyShift action_7
action_84 (28) = happyShift action_8
action_84 (29) = happyShift action_9
action_84 (30) = happyShift action_10
action_84 (34) = happyShift action_11
action_84 (12) = happyGoto action_92
action_84 _ = happyFail

action_85 (17) = happyShift action_90
action_85 (33) = happyShift action_91
action_85 _ = happyFail

action_86 (16) = happyShift action_3
action_86 (5) = happyGoto action_89
action_86 _ = happyFail

action_87 (16) = happyShift action_33
action_87 (20) = happyShift action_34
action_87 (24) = happyShift action_35
action_87 (29) = happyShift action_36
action_87 (7) = happyGoto action_88
action_87 _ = happyFail

action_88 (16) = happyShift action_51
action_88 (19) = happyShift action_52
action_88 (24) = happyShift action_54
action_88 _ = happyReduce_10

action_89 _ = happyReduce_3

action_90 (16) = happyShift action_3
action_90 (5) = happyGoto action_93
action_90 _ = happyFail

action_91 _ = happyReduce_12

action_92 (16) = happyShift action_28
action_92 (22) = happyShift action_29
action_92 (24) = happyShift action_30
action_92 _ = happyReduce_17

action_93 _ = happyReduce_4

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn5
		 (TBind (Just happy_var_1) happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn6
		 ([happy_var_2]
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn7
		 (TyVar happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn7
		 (TySet
	)

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (TyApp happy_var_1 (TyVar happy_var_2)
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 7 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TyApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (TyFun (TBind Nothing happy_var_1) happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 5 7 happyReduction_10
happyReduction_10 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TyFun happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 5 7 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TyInd (TBind (Just happy_var_2) TySet) happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  8 happyReduction_14
happyReduction_14 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  9 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn9
		 ([happy_var_2]
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  9 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  10 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn10
		 (TAlt happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn6
		 ([happy_var_2]
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 4 11 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (happy_var_1 ++ [happy_var_3]
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_1  12 happyReduction_20
happyReduction_20 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn12
		 (TVar happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 12 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInj happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TInj happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_2  12 happyReduction_22
happyReduction_22 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (TApp happy_var_1 (TVar happy_var_2)
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 4 12 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 4 12 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TApp happy_var_1 (TType happy_var_3)
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  12 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 4 12 happyReduction_26
happyReduction_26 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 4 12 happyReduction_27
happyReduction_27 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TFix happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 5 12 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TCase happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 6 12 happyReduction_29
happyReduction_29 ((HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TLet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 4 13 happyReduction_30
happyReduction_30 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 4 14 happyReduction_31
happyReduction_31 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn15
		 (Program [] [happy_var_1]
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  15 happyReduction_33
happyReduction_33 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 (Program [happy_var_1] []
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  15 happyReduction_34
happyReduction_34 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (modify programTypes (++ [happy_var_2]) happy_var_1
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  15 happyReduction_35
happyReduction_35 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (modify programTerms (++ [happy_var_2]) happy_var_1
	)
happyReduction_35 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 35 35 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 16;
	TokenBar -> cont 17;
	TokenTypeOf -> cont 18;
	TokenArr -> cont 19;
	TokenSet -> cont 20;
	TokenEq -> cont 21;
	TokenOS -> cont 22;
	TokenCS -> cont 23;
	TokenOP -> cont 24;
	TokenCP -> cont 25;
	TokenMatch -> cont 26;
	TokenWith -> cont 27;
	TokenFun -> cont 28;
	TokenFix -> cont 29;
	TokenLet -> cont 30;
	TokenIn -> cont 31;
	TokenType -> cont 32;
	TokenEnd -> cont 33;
	TokenInj happy_dollar_dollar -> cont 34;
	_ -> happyError' (tk:tks)
	}

happyError_ 35 tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn15 z -> happyReturn z; _other -> notHappyAtAll })

happyTerm tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn12 z -> happyReturn z; _other -> notHappyAtAll })

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
term = 
    withEmptyScope 
  . parseRawTerm 
  . happyTerm 
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
