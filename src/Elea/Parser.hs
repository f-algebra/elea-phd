{-# OPTIONS_GHC -w #-}
module Elea.Parser 
(
  program, term
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Index
import Elea.Show ( showM )
import qualified Elea.Term as Term
import qualified Elea.Typing as Typing
import qualified Elea.Simplifier as Simp
import qualified Elea.Env as Env
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Error as Err
import qualified Data.Map as Map

type TermDef = (String, RawTerm)
type RawProgram = [TermDef]

data RawBind 
  = TBind { _rawLabel :: Maybe String
          , _rawType :: RawTerm }

data RawTerm
  = TVar String
  | TApp RawTerm RawTerm
  | TFix [RawBind] RawTerm
  | TLam [RawBind] RawTerm
  | TInj Nat RawTerm
  | TInd RawBind [RawBind]
  | TAny [RawBind] RawTerm
  | TPi [RawBind] RawTerm
  | TAbsurd
  | TSet
  | TCase RawTerm [RawAlt]
  | TLet String RawTerm RawTerm
  
data RawAlt
  = TAlt [String] RawTerm
  
data Scope 
  = Scope { _bindMap :: Map String (Either Term Index)
          , _bindStack :: [Bind] }
  
mkLabels [''Scope, ''RawBind]

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn5 (RawBind)
	| HappyAbsSyn7 ([RawBind])
	| HappyAbsSyn8 ([String])
	| HappyAbsSyn9 ([RawAlt])
	| HappyAbsSyn10 (RawAlt)
	| HappyAbsSyn12 (RawTerm)
	| HappyAbsSyn13 (TermDef)
	| HappyAbsSyn14 (RawProgram)

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
 action_82 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_31 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (30) = happyShift action_18
action_0 (13) = happyGoto action_16
action_0 (14) = happyGoto action_17
action_0 _ = happyFail

action_1 (15) = happyShift action_5
action_1 (20) = happyShift action_6
action_1 (24) = happyShift action_7
action_1 (26) = happyShift action_8
action_1 (28) = happyShift action_9
action_1 (29) = happyShift action_10
action_1 (30) = happyShift action_11
action_1 (34) = happyShift action_12
action_1 (35) = happyShift action_13
action_1 (36) = happyShift action_14
action_1 (37) = happyShift action_15
action_1 (12) = happyGoto action_4
action_1 _ = happyFail

action_2 (15) = happyShift action_3
action_2 _ = happyFail

action_3 (17) = happyShift action_36
action_3 _ = happyFail

action_4 (15) = happyShift action_34
action_4 (24) = happyShift action_35
action_4 (38) = happyAccept
action_4 _ = happyFail

action_5 _ = happyReduce_15

action_6 _ = happyReduce_16

action_7 (15) = happyShift action_5
action_7 (20) = happyShift action_6
action_7 (24) = happyShift action_7
action_7 (26) = happyShift action_8
action_7 (28) = happyShift action_9
action_7 (29) = happyShift action_10
action_7 (30) = happyShift action_11
action_7 (34) = happyShift action_12
action_7 (35) = happyShift action_13
action_7 (36) = happyShift action_14
action_7 (37) = happyShift action_15
action_7 (12) = happyGoto action_33
action_7 _ = happyFail

action_8 (15) = happyShift action_5
action_8 (20) = happyShift action_6
action_8 (24) = happyShift action_7
action_8 (26) = happyShift action_8
action_8 (28) = happyShift action_9
action_8 (29) = happyShift action_10
action_8 (30) = happyShift action_11
action_8 (34) = happyShift action_12
action_8 (35) = happyShift action_13
action_8 (36) = happyShift action_14
action_8 (37) = happyShift action_15
action_8 (12) = happyGoto action_32
action_8 _ = happyFail

action_9 (15) = happyShift action_23
action_9 (24) = happyShift action_24
action_9 (6) = happyGoto action_21
action_9 (11) = happyGoto action_31
action_9 _ = happyFail

action_10 (15) = happyShift action_23
action_10 (24) = happyShift action_24
action_10 (6) = happyGoto action_21
action_10 (11) = happyGoto action_30
action_10 _ = happyFail

action_11 (15) = happyShift action_29
action_11 _ = happyFail

action_12 (15) = happyShift action_5
action_12 (20) = happyShift action_6
action_12 (24) = happyShift action_7
action_12 (26) = happyShift action_8
action_12 (28) = happyShift action_9
action_12 (29) = happyShift action_10
action_12 (30) = happyShift action_11
action_12 (34) = happyShift action_12
action_12 (35) = happyShift action_13
action_12 (36) = happyShift action_14
action_12 (37) = happyShift action_15
action_12 (12) = happyGoto action_28
action_12 _ = happyFail

action_13 (15) = happyShift action_27
action_13 (5) = happyGoto action_26
action_13 _ = happyFail

action_14 (15) = happyShift action_23
action_14 (24) = happyShift action_24
action_14 (6) = happyGoto action_21
action_14 (11) = happyGoto action_25
action_14 _ = happyFail

action_15 (15) = happyShift action_23
action_15 (24) = happyShift action_24
action_15 (6) = happyGoto action_21
action_15 (11) = happyGoto action_22
action_15 _ = happyFail

action_16 _ = happyReduce_30

action_17 (30) = happyShift action_18
action_17 (38) = happyAccept
action_17 (13) = happyGoto action_20
action_17 _ = happyFail

action_18 (15) = happyShift action_19
action_18 _ = happyFail

action_19 (21) = happyShift action_52
action_19 _ = happyFail

action_20 _ = happyReduce_31

action_21 _ = happyReduce_13

action_22 (15) = happyShift action_23
action_22 (18) = happyShift action_51
action_22 (24) = happyShift action_24
action_22 (6) = happyGoto action_41
action_22 _ = happyFail

action_23 _ = happyReduce_3

action_24 (15) = happyShift action_50
action_24 (20) = happyShift action_6
action_24 (24) = happyShift action_7
action_24 (26) = happyShift action_8
action_24 (28) = happyShift action_9
action_24 (29) = happyShift action_10
action_24 (30) = happyShift action_11
action_24 (34) = happyShift action_12
action_24 (35) = happyShift action_13
action_24 (36) = happyShift action_14
action_24 (37) = happyShift action_15
action_24 (5) = happyGoto action_48
action_24 (12) = happyGoto action_49
action_24 _ = happyFail

action_25 (15) = happyShift action_23
action_25 (18) = happyShift action_47
action_25 (24) = happyShift action_24
action_25 (6) = happyGoto action_41
action_25 _ = happyFail

action_26 (27) = happyShift action_46
action_26 _ = happyFail

action_27 (17) = happyShift action_36
action_27 (27) = happyShift action_45
action_27 _ = happyFail

action_28 (15) = happyShift action_34
action_28 (24) = happyShift action_35
action_28 _ = happyReduce_20

action_29 (21) = happyShift action_44
action_29 _ = happyFail

action_30 (15) = happyShift action_23
action_30 (18) = happyShift action_43
action_30 (24) = happyShift action_24
action_30 (6) = happyGoto action_41
action_30 _ = happyFail

action_31 (15) = happyShift action_23
action_31 (18) = happyShift action_42
action_31 (24) = happyShift action_24
action_31 (6) = happyGoto action_41
action_31 _ = happyFail

action_32 (15) = happyShift action_34
action_32 (24) = happyShift action_35
action_32 (27) = happyShift action_40
action_32 _ = happyFail

action_33 (15) = happyShift action_34
action_33 (24) = happyShift action_35
action_33 (25) = happyShift action_39
action_33 _ = happyFail

action_34 _ = happyReduce_17

action_35 (15) = happyShift action_5
action_35 (20) = happyShift action_6
action_35 (24) = happyShift action_7
action_35 (26) = happyShift action_8
action_35 (28) = happyShift action_9
action_35 (29) = happyShift action_10
action_35 (30) = happyShift action_11
action_35 (34) = happyShift action_12
action_35 (35) = happyShift action_13
action_35 (36) = happyShift action_14
action_35 (37) = happyShift action_15
action_35 (12) = happyGoto action_38
action_35 _ = happyFail

action_36 (15) = happyShift action_5
action_36 (20) = happyShift action_6
action_36 (24) = happyShift action_7
action_36 (26) = happyShift action_8
action_36 (28) = happyShift action_9
action_36 (29) = happyShift action_10
action_36 (30) = happyShift action_11
action_36 (34) = happyShift action_12
action_36 (35) = happyShift action_13
action_36 (36) = happyShift action_14
action_36 (37) = happyShift action_15
action_36 (12) = happyGoto action_37
action_36 _ = happyFail

action_37 (15) = happyShift action_34
action_37 (24) = happyShift action_35
action_37 _ = happyReduce_2

action_38 (15) = happyShift action_34
action_38 (24) = happyShift action_35
action_38 (25) = happyShift action_66
action_38 _ = happyFail

action_39 _ = happyReduce_19

action_40 (16) = happyShift action_65
action_40 (9) = happyGoto action_64
action_40 _ = happyFail

action_41 _ = happyReduce_14

action_42 (15) = happyShift action_5
action_42 (20) = happyShift action_6
action_42 (24) = happyShift action_7
action_42 (26) = happyShift action_8
action_42 (28) = happyShift action_9
action_42 (29) = happyShift action_10
action_42 (30) = happyShift action_11
action_42 (34) = happyShift action_12
action_42 (35) = happyShift action_13
action_42 (36) = happyShift action_14
action_42 (37) = happyShift action_15
action_42 (12) = happyGoto action_63
action_42 _ = happyFail

action_43 (15) = happyShift action_5
action_43 (20) = happyShift action_6
action_43 (24) = happyShift action_7
action_43 (26) = happyShift action_8
action_43 (28) = happyShift action_9
action_43 (29) = happyShift action_10
action_43 (30) = happyShift action_11
action_43 (34) = happyShift action_12
action_43 (35) = happyShift action_13
action_43 (36) = happyShift action_14
action_43 (37) = happyShift action_15
action_43 (12) = happyGoto action_62
action_43 _ = happyFail

action_44 (15) = happyShift action_5
action_44 (20) = happyShift action_6
action_44 (24) = happyShift action_7
action_44 (26) = happyShift action_8
action_44 (28) = happyShift action_9
action_44 (29) = happyShift action_10
action_44 (30) = happyShift action_11
action_44 (34) = happyShift action_12
action_44 (35) = happyShift action_13
action_44 (36) = happyShift action_14
action_44 (37) = happyShift action_15
action_44 (12) = happyGoto action_61
action_44 _ = happyFail

action_45 (16) = happyShift action_59
action_45 (7) = happyGoto action_60
action_45 _ = happyFail

action_46 (16) = happyShift action_59
action_46 (7) = happyGoto action_58
action_46 _ = happyFail

action_47 (15) = happyShift action_5
action_47 (20) = happyShift action_6
action_47 (24) = happyShift action_7
action_47 (26) = happyShift action_8
action_47 (28) = happyShift action_9
action_47 (29) = happyShift action_10
action_47 (30) = happyShift action_11
action_47 (34) = happyShift action_12
action_47 (35) = happyShift action_13
action_47 (36) = happyShift action_14
action_47 (37) = happyShift action_15
action_47 (12) = happyGoto action_57
action_47 _ = happyFail

action_48 (25) = happyShift action_56
action_48 _ = happyFail

action_49 (15) = happyShift action_34
action_49 (24) = happyShift action_35
action_49 (25) = happyShift action_55
action_49 _ = happyFail

action_50 (17) = happyShift action_36
action_50 _ = happyReduce_15

action_51 (15) = happyShift action_5
action_51 (20) = happyShift action_6
action_51 (24) = happyShift action_7
action_51 (26) = happyShift action_8
action_51 (28) = happyShift action_9
action_51 (29) = happyShift action_10
action_51 (30) = happyShift action_11
action_51 (34) = happyShift action_12
action_51 (35) = happyShift action_13
action_51 (36) = happyShift action_14
action_51 (37) = happyShift action_15
action_51 (12) = happyGoto action_54
action_51 _ = happyFail

action_52 (15) = happyShift action_5
action_52 (20) = happyShift action_6
action_52 (24) = happyShift action_7
action_52 (26) = happyShift action_8
action_52 (28) = happyShift action_9
action_52 (29) = happyShift action_10
action_52 (30) = happyShift action_11
action_52 (34) = happyShift action_12
action_52 (35) = happyShift action_13
action_52 (36) = happyShift action_14
action_52 (37) = happyShift action_15
action_52 (12) = happyGoto action_53
action_52 _ = happyFail

action_53 (15) = happyShift action_34
action_53 (24) = happyShift action_35
action_53 _ = happyReduce_29

action_54 (15) = happyShift action_34
action_54 (24) = happyShift action_35
action_54 _ = happyReduce_24

action_55 _ = happyReduce_4

action_56 _ = happyReduce_5

action_57 (15) = happyShift action_34
action_57 (24) = happyShift action_35
action_57 _ = happyReduce_23

action_58 (16) = happyShift action_73
action_58 (33) = happyShift action_76
action_58 _ = happyFail

action_59 (15) = happyShift action_3
action_59 (5) = happyGoto action_75
action_59 _ = happyFail

action_60 (16) = happyShift action_73
action_60 (33) = happyShift action_74
action_60 _ = happyFail

action_61 (15) = happyShift action_34
action_61 (24) = happyShift action_35
action_61 (31) = happyShift action_72
action_61 _ = happyFail

action_62 (15) = happyShift action_34
action_62 (24) = happyShift action_35
action_62 _ = happyReduce_22

action_63 (15) = happyShift action_34
action_63 (24) = happyShift action_35
action_63 _ = happyReduce_21

action_64 (16) = happyShift action_70
action_64 (33) = happyShift action_71
action_64 _ = happyFail

action_65 (15) = happyShift action_69
action_65 (8) = happyGoto action_67
action_65 (10) = happyGoto action_68
action_65 _ = happyFail

action_66 _ = happyReduce_18

action_67 (15) = happyShift action_80
action_67 (18) = happyShift action_81
action_67 _ = happyFail

action_68 _ = happyReduce_10

action_69 _ = happyReduce_8

action_70 (15) = happyShift action_69
action_70 (8) = happyGoto action_67
action_70 (10) = happyGoto action_79
action_70 _ = happyFail

action_71 _ = happyReduce_26

action_72 (15) = happyShift action_5
action_72 (20) = happyShift action_6
action_72 (24) = happyShift action_7
action_72 (26) = happyShift action_8
action_72 (28) = happyShift action_9
action_72 (29) = happyShift action_10
action_72 (30) = happyShift action_11
action_72 (34) = happyShift action_12
action_72 (35) = happyShift action_13
action_72 (36) = happyShift action_14
action_72 (37) = happyShift action_15
action_72 (12) = happyGoto action_78
action_72 _ = happyFail

action_73 (15) = happyShift action_3
action_73 (5) = happyGoto action_77
action_73 _ = happyFail

action_74 _ = happyReduce_27

action_75 _ = happyReduce_6

action_76 _ = happyReduce_28

action_77 _ = happyReduce_7

action_78 (15) = happyShift action_34
action_78 (24) = happyShift action_35
action_78 _ = happyReduce_25

action_79 _ = happyReduce_11

action_80 _ = happyReduce_9

action_81 (15) = happyShift action_5
action_81 (20) = happyShift action_6
action_81 (24) = happyShift action_7
action_81 (26) = happyShift action_8
action_81 (28) = happyShift action_9
action_81 (29) = happyShift action_10
action_81 (30) = happyShift action_11
action_81 (34) = happyShift action_12
action_81 (35) = happyShift action_13
action_81 (36) = happyShift action_14
action_81 (37) = happyShift action_15
action_81 (12) = happyGoto action_82
action_81 _ = happyFail

action_82 (15) = happyShift action_34
action_82 (24) = happyShift action_35
action_82 _ = happyReduce_12

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn5
		 (TBind (Just happy_var_1) happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn5
		 (TBind Nothing (TVar happy_var_1)
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (TBind Nothing happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn7
		 ([happy_var_2]
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn9
		 ([happy_var_2]
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn10
		 (TAlt happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  11 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  12 happyReduction_15
happyReduction_15 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn12
		 (TVar happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  12 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn12
		 (TSet
	)

happyReduce_17 = happySpecReduce_2  12 happyReduction_17
happyReduction_17 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (TApp happy_var_1 (TVar happy_var_2)
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 12 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  12 happyReduction_20
happyReduction_20 (HappyAbsSyn12  happy_var_2)
	(HappyTerminal (TokenInj happy_var_1))
	 =  HappyAbsSyn12
		 (TInj happy_var_1 happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 12 happyReduction_21
happyReduction_21 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 4 12 happyReduction_22
happyReduction_22 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TFix happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 4 12 happyReduction_23
happyReduction_23 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TAny happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 4 12 happyReduction_24
happyReduction_24 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TPi happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 6 12 happyReduction_25
happyReduction_25 ((HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TLet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 5 12 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TCase happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 5 12 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TInd (TBind (Just happy_var_2) TSet) happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 5 12 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TInd happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 4 13 happyReduction_29
happyReduction_29 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_1  14 happyReduction_30
happyReduction_30 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  14 happyReduction_31
happyReduction_31 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_31 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 38 38 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 15;
	TokenBar -> cont 16;
	TokenTypeOf -> cont 17;
	TokenArr -> cont 18;
	TokenDArr -> cont 19;
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
	TokenInd -> cont 35;
	TokenAny -> cont 36;
	TokenPi -> cont 37;
	_ -> happyError' (tk:tks)
	}

happyError_ 38 tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn14 z -> happyReturn z; _other -> notHappyAtAll })

happyTerm tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn12 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


withEmptyScope :: ReaderT Scope m a -> m a
withEmptyScope = flip runReaderT (Scope mempty mempty)

instance Monad m => Env.Writable (ReaderT Scope m) where
  bindAt at b = 
      local
    $ modify bindMap (addToMap . map (liftAt at))
    . modify bindStack (addToStack . map (liftAt at))
    where
    addToStack = 
      insertAt (convertEnum at) (liftAt at b)
      
    addToMap 
      | Bind (Just lbl) _ <- b = 
          Map.insert lbl (Right at)
      | otherwise = id
      
  equals _ _ = id

instance Err.Monad m => Env.Readable (ReaderT Scope m) where
  bindings = asks (get bindStack)
  matches = return mempty
  
type ParserMonad m a = (Err.Monad m, Defs.Monad m) => ReaderT Scope m a
    
localDef :: MonadReader Scope m => String -> Term -> m a -> m a
localDef name term = 
    local 
  $ modify bindMap 
  $ Map.insert name (Left term)
  
term :: (Err.Monad m, Defs.Monad m) => String -> m Term
term = 
    withEmptyScope 
  . parseAndCheckTerm 
  . happyTerm 
  . lexer
  
program :: (Err.Monad m, Defs.Monad m) => String -> m ()
program = 
    withEmptyScope
  . mapM_ define
  . happyProgram 
  . lexer
  where
  define (name, raw_term) = do
    term <- parseAndCheckTerm raw_term
    Defs.add name term
    
lookupTerm :: String -> ParserMonad m Term
lookupTerm name = do
  mby_bind <- asks (Map.lookup name . get bindMap)
  case mby_bind of
    Just (Left term) -> return term
    Just (Right idx) -> return (Term.Var idx)
    Nothing -> do
      mby_term <- Defs.lookup name
      if (isNothing mby_term)
      then Err.throw $ "Undefined term: " ++ name
      else return (fromJust mby_term)
      
parseAndCheckTerm :: RawTerm -> ParserMonad m Term
parseAndCheckTerm =
    Err.check Typing.check
  . liftM Simp.safe
  . parseRawTerm

parseRawTerm :: RawTerm -> ParserMonad m Term
parseRawTerm TAbsurd = return Absurd
parseRawTerm TSet = return Set
parseRawTerm (TVar var) = lookupTerm var
parseRawTerm (TInj n rty) = do
  ty <- parseRawTerm rty
  return (Inj n ty)
parseRawTerm (TApp rt1 rt2) = do
  t1 <- parseRawTerm rt1 
  t2 <- parseRawTerm rt2
  return (Simp.safe (App t1 t2))
parseRawTerm (TFix rbs rt) = do
  bs <- parseRawBinds rbs
  t <- Env.bindMany bs (parseRawTerm rt)
  return 
    $ Fix (head bs) 
    $ unflattenLam (tail bs) t 
parseRawTerm (TLam rbs rt) = do
  bs <- parseRawBinds rbs
  t <- Env.bindMany bs (parseRawTerm rt)
  return (unflattenLam bs t)
parseRawTerm (TPi rbs rt) = do
  bs <- parseRawBinds rbs
  t <- Env.bindMany bs (parseRawTerm rt)
  return (unflattenPi bs t)
parseRawTerm (TAny rbs rt) = do
  bs <- parseRawBinds rbs
  Env.bindMany bs (parseRawTerm rt)
parseRawTerm (TLet name rt1 rt2) = do
  t1 <- parseRawTerm rt1
  localDef name t1 (parseRawTerm rt2)
parseRawTerm (TInd rb rcons) = do
  b <- parseRawBind rb
  cons <- Env.bind b (mapM parseRawBind rcons)
  return (Ind b cons)
parseRawTerm (TCase rt ralts) = do
  t <- parseRawTerm rt
  ind_ty <- Typing.typeOf t
  alts <- mapM (parseRawAlt ind_ty) ralts
  return (Case t ind_ty alts)
  where
  parseRawAlt ind_ty raw_alt = do
    t <- Env.bindMany var_bs (parseRawTerm rt)
    return (Alt var_bs t)
    where
    TAlt (con_lbl:var_lbls) rt = raw_alt
    cons = Typing.unfoldInd ind_ty

    -- Find the type of this particular constructor from looking it up
    -- by name in the description of the inductive type.
    Just this_con = find ((== Just con_lbl) . get boundLabel) cons

    -- The bindings for this constructor are the arguments for the type
    con_bs = (fst . flattenPi . get boundType) this_con
    
    -- The new variable bindings are the constructor type bindings, 
    -- renamed to the labels given in the pattern match
    var_bs = zipWith (set boundLabel . Just) var_lbls con_bs
  
parseRawBinds :: [RawBind] -> ParserMonad m [Bind]
parseRawBinds [] = return []
parseRawBinds (rb:rbs) = do
  b <- parseRawBind rb
  -- Make sure each binding is applied to the latter ones
  liftM (b:)
    $ Env.bind b
    $ parseRawBinds rbs

parseRawBind :: RawBind -> ParserMonad m Bind
parseRawBind (TBind label raw_ty) = do
  ty <- parseRawTerm raw_ty
  return (Bind label ty)

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
  | TokenAny
  | TokenPi
  
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
lexer ('=':'>':cs) = TokenDArr : lexer cs
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
      ("ind", rest) -> TokenInd : lexer rest
      ("end", rest) -> TokenEnd : lexer rest
      ("any", rest) -> TokenAny : lexer rest
      ("pi", rest) -> TokenPi : lexer rest
      (name, rest) -> TokenName name : lexer rest
lexer cs = error $ "Unrecognized symbol " ++ take 1 cs

instance Show Token where
  show TokenBar = "|"
  show (TokenName x) = x
  show TokenTypeOf = ":"
  show TokenSet = "*"
  show TokenArr = "->"
  show TokenDArr = "=>"
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
  show TokenInd = "ind"
  show TokenType = "type"
  show TokenEnd = "end"
  show (TokenInj n) = "inj" ++ show n
  show TokenAny = "any"
  show TokenPi = "pi"
  
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
