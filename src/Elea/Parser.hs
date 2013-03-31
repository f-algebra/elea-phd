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
	| HappyAbsSyn6 ([RawBind])
	| HappyAbsSyn7 ([String])
	| HappyAbsSyn8 ([RawAlt])
	| HappyAbsSyn9 (RawAlt)
	| HappyAbsSyn11 (RawTerm)
	| HappyAbsSyn12 (TermDef)
	| HappyAbsSyn13 (RawProgram)

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
 action_77 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_28 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (29) = happyShift action_18
action_0 (12) = happyGoto action_16
action_0 (13) = happyGoto action_17
action_0 _ = happyFail

action_1 (14) = happyShift action_5
action_1 (19) = happyShift action_6
action_1 (23) = happyShift action_7
action_1 (25) = happyShift action_8
action_1 (27) = happyShift action_9
action_1 (28) = happyShift action_10
action_1 (29) = happyShift action_11
action_1 (33) = happyShift action_12
action_1 (34) = happyShift action_13
action_1 (35) = happyShift action_14
action_1 (36) = happyShift action_15
action_1 (11) = happyGoto action_4
action_1 _ = happyFail

action_2 (14) = happyShift action_3
action_2 _ = happyFail

action_3 (16) = happyShift action_34
action_3 _ = happyFail

action_4 (14) = happyShift action_31
action_4 (17) = happyShift action_32
action_4 (23) = happyShift action_33
action_4 (37) = happyAccept
action_4 _ = happyFail

action_5 _ = happyReduce_12

action_6 _ = happyReduce_13

action_7 (14) = happyShift action_5
action_7 (19) = happyShift action_6
action_7 (23) = happyShift action_7
action_7 (25) = happyShift action_8
action_7 (27) = happyShift action_9
action_7 (28) = happyShift action_10
action_7 (29) = happyShift action_11
action_7 (33) = happyShift action_12
action_7 (34) = happyShift action_13
action_7 (35) = happyShift action_14
action_7 (36) = happyShift action_15
action_7 (11) = happyGoto action_30
action_7 _ = happyFail

action_8 (14) = happyShift action_5
action_8 (19) = happyShift action_6
action_8 (23) = happyShift action_7
action_8 (25) = happyShift action_8
action_8 (27) = happyShift action_9
action_8 (28) = happyShift action_10
action_8 (29) = happyShift action_11
action_8 (33) = happyShift action_12
action_8 (34) = happyShift action_13
action_8 (35) = happyShift action_14
action_8 (36) = happyShift action_15
action_8 (11) = happyGoto action_29
action_8 _ = happyFail

action_9 (23) = happyShift action_22
action_9 (10) = happyGoto action_28
action_9 _ = happyFail

action_10 (23) = happyShift action_22
action_10 (10) = happyGoto action_27
action_10 _ = happyFail

action_11 (14) = happyShift action_26
action_11 _ = happyFail

action_12 (14) = happyShift action_5
action_12 (19) = happyShift action_6
action_12 (23) = happyShift action_7
action_12 (25) = happyShift action_8
action_12 (27) = happyShift action_9
action_12 (28) = happyShift action_10
action_12 (29) = happyShift action_11
action_12 (33) = happyShift action_12
action_12 (34) = happyShift action_13
action_12 (35) = happyShift action_14
action_12 (36) = happyShift action_15
action_12 (11) = happyGoto action_25
action_12 _ = happyFail

action_13 (14) = happyShift action_3
action_13 (5) = happyGoto action_24
action_13 _ = happyFail

action_14 (23) = happyShift action_22
action_14 (10) = happyGoto action_23
action_14 _ = happyFail

action_15 (23) = happyShift action_22
action_15 (10) = happyGoto action_21
action_15 _ = happyFail

action_16 _ = happyReduce_27

action_17 (29) = happyShift action_18
action_17 (37) = happyAccept
action_17 (12) = happyGoto action_20
action_17 _ = happyFail

action_18 (14) = happyShift action_19
action_18 _ = happyFail

action_19 (20) = happyShift action_48
action_19 _ = happyFail

action_20 _ = happyReduce_28

action_21 (18) = happyShift action_47
action_21 (23) = happyShift action_41
action_21 _ = happyFail

action_22 (14) = happyShift action_3
action_22 (5) = happyGoto action_46
action_22 _ = happyFail

action_23 (18) = happyShift action_45
action_23 (23) = happyShift action_41
action_23 _ = happyFail

action_24 (26) = happyShift action_44
action_24 _ = happyFail

action_25 (14) = happyShift action_31
action_25 (17) = happyShift action_32
action_25 (23) = happyShift action_33
action_25 _ = happyReduce_14

action_26 (20) = happyShift action_43
action_26 _ = happyFail

action_27 (18) = happyShift action_42
action_27 (23) = happyShift action_41
action_27 _ = happyFail

action_28 (18) = happyShift action_40
action_28 (23) = happyShift action_41
action_28 _ = happyFail

action_29 (14) = happyShift action_31
action_29 (17) = happyShift action_32
action_29 (23) = happyShift action_33
action_29 (26) = happyShift action_39
action_29 _ = happyFail

action_30 (14) = happyShift action_31
action_30 (17) = happyShift action_32
action_30 (23) = happyShift action_33
action_30 (24) = happyShift action_38
action_30 _ = happyFail

action_31 _ = happyReduce_15

action_32 (14) = happyShift action_5
action_32 (19) = happyShift action_6
action_32 (23) = happyShift action_7
action_32 (25) = happyShift action_8
action_32 (27) = happyShift action_9
action_32 (28) = happyShift action_10
action_32 (29) = happyShift action_11
action_32 (33) = happyShift action_12
action_32 (34) = happyShift action_13
action_32 (35) = happyShift action_14
action_32 (36) = happyShift action_15
action_32 (11) = happyGoto action_37
action_32 _ = happyFail

action_33 (14) = happyShift action_5
action_33 (19) = happyShift action_6
action_33 (23) = happyShift action_7
action_33 (25) = happyShift action_8
action_33 (27) = happyShift action_9
action_33 (28) = happyShift action_10
action_33 (29) = happyShift action_11
action_33 (33) = happyShift action_12
action_33 (34) = happyShift action_13
action_33 (35) = happyShift action_14
action_33 (36) = happyShift action_15
action_33 (11) = happyGoto action_36
action_33 _ = happyFail

action_34 (14) = happyShift action_5
action_34 (19) = happyShift action_6
action_34 (23) = happyShift action_7
action_34 (25) = happyShift action_8
action_34 (27) = happyShift action_9
action_34 (28) = happyShift action_10
action_34 (29) = happyShift action_11
action_34 (33) = happyShift action_12
action_34 (34) = happyShift action_13
action_34 (35) = happyShift action_14
action_34 (36) = happyShift action_15
action_34 (11) = happyGoto action_35
action_34 _ = happyFail

action_35 (14) = happyShift action_31
action_35 (17) = happyShift action_32
action_35 (23) = happyShift action_33
action_35 _ = happyReduce_2

action_36 (14) = happyShift action_31
action_36 (17) = happyShift action_32
action_36 (23) = happyShift action_33
action_36 (24) = happyShift action_61
action_36 _ = happyFail

action_37 (14) = happyShift action_31
action_37 (17) = happyShift action_32
action_37 (23) = happyShift action_33
action_37 _ = happyReduce_18

action_38 _ = happyReduce_20

action_39 (15) = happyShift action_60
action_39 (8) = happyGoto action_59
action_39 _ = happyFail

action_40 (14) = happyShift action_5
action_40 (19) = happyShift action_6
action_40 (23) = happyShift action_7
action_40 (25) = happyShift action_8
action_40 (27) = happyShift action_9
action_40 (28) = happyShift action_10
action_40 (29) = happyShift action_11
action_40 (33) = happyShift action_12
action_40 (34) = happyShift action_13
action_40 (35) = happyShift action_14
action_40 (36) = happyShift action_15
action_40 (11) = happyGoto action_58
action_40 _ = happyFail

action_41 (14) = happyShift action_3
action_41 (5) = happyGoto action_57
action_41 _ = happyFail

action_42 (14) = happyShift action_5
action_42 (19) = happyShift action_6
action_42 (23) = happyShift action_7
action_42 (25) = happyShift action_8
action_42 (27) = happyShift action_9
action_42 (28) = happyShift action_10
action_42 (29) = happyShift action_11
action_42 (33) = happyShift action_12
action_42 (34) = happyShift action_13
action_42 (35) = happyShift action_14
action_42 (36) = happyShift action_15
action_42 (11) = happyGoto action_56
action_42 _ = happyFail

action_43 (14) = happyShift action_5
action_43 (19) = happyShift action_6
action_43 (23) = happyShift action_7
action_43 (25) = happyShift action_8
action_43 (27) = happyShift action_9
action_43 (28) = happyShift action_10
action_43 (29) = happyShift action_11
action_43 (33) = happyShift action_12
action_43 (34) = happyShift action_13
action_43 (35) = happyShift action_14
action_43 (36) = happyShift action_15
action_43 (11) = happyGoto action_55
action_43 _ = happyFail

action_44 (15) = happyShift action_54
action_44 (6) = happyGoto action_53
action_44 _ = happyFail

action_45 (14) = happyShift action_5
action_45 (19) = happyShift action_6
action_45 (23) = happyShift action_7
action_45 (25) = happyShift action_8
action_45 (27) = happyShift action_9
action_45 (28) = happyShift action_10
action_45 (29) = happyShift action_11
action_45 (33) = happyShift action_12
action_45 (34) = happyShift action_13
action_45 (35) = happyShift action_14
action_45 (36) = happyShift action_15
action_45 (11) = happyGoto action_52
action_45 _ = happyFail

action_46 (24) = happyShift action_51
action_46 _ = happyFail

action_47 (14) = happyShift action_5
action_47 (19) = happyShift action_6
action_47 (23) = happyShift action_7
action_47 (25) = happyShift action_8
action_47 (27) = happyShift action_9
action_47 (28) = happyShift action_10
action_47 (29) = happyShift action_11
action_47 (33) = happyShift action_12
action_47 (34) = happyShift action_13
action_47 (35) = happyShift action_14
action_47 (36) = happyShift action_15
action_47 (11) = happyGoto action_50
action_47 _ = happyFail

action_48 (14) = happyShift action_5
action_48 (19) = happyShift action_6
action_48 (23) = happyShift action_7
action_48 (25) = happyShift action_8
action_48 (27) = happyShift action_9
action_48 (28) = happyShift action_10
action_48 (29) = happyShift action_11
action_48 (33) = happyShift action_12
action_48 (34) = happyShift action_13
action_48 (35) = happyShift action_14
action_48 (36) = happyShift action_15
action_48 (11) = happyGoto action_49
action_48 _ = happyFail

action_49 (14) = happyShift action_31
action_49 (17) = happyShift action_32
action_49 (23) = happyShift action_33
action_49 _ = happyReduce_26

action_50 (14) = happyShift action_31
action_50 (17) = happyShift action_32
action_50 (23) = happyShift action_33
action_50 _ = happyReduce_24

action_51 _ = happyReduce_10

action_52 (14) = happyShift action_31
action_52 (17) = happyShift action_32
action_52 (23) = happyShift action_33
action_52 _ = happyReduce_23

action_53 (15) = happyShift action_70
action_53 (32) = happyShift action_71
action_53 _ = happyFail

action_54 (14) = happyShift action_3
action_54 (5) = happyGoto action_69
action_54 _ = happyFail

action_55 (14) = happyShift action_31
action_55 (17) = happyShift action_32
action_55 (23) = happyShift action_33
action_55 (30) = happyShift action_68
action_55 _ = happyFail

action_56 (14) = happyShift action_31
action_56 (17) = happyShift action_32
action_56 (23) = happyShift action_33
action_56 _ = happyReduce_22

action_57 (24) = happyShift action_67
action_57 _ = happyFail

action_58 (14) = happyShift action_31
action_58 (17) = happyShift action_32
action_58 (23) = happyShift action_33
action_58 _ = happyReduce_21

action_59 (15) = happyShift action_65
action_59 (32) = happyShift action_66
action_59 _ = happyFail

action_60 (14) = happyShift action_64
action_60 (7) = happyGoto action_62
action_60 (9) = happyGoto action_63
action_60 _ = happyFail

action_61 _ = happyReduce_19

action_62 (14) = happyShift action_75
action_62 (18) = happyShift action_76
action_62 _ = happyFail

action_63 _ = happyReduce_7

action_64 _ = happyReduce_5

action_65 (14) = happyShift action_64
action_65 (7) = happyGoto action_62
action_65 (9) = happyGoto action_74
action_65 _ = happyFail

action_66 _ = happyReduce_16

action_67 _ = happyReduce_11

action_68 (14) = happyShift action_5
action_68 (19) = happyShift action_6
action_68 (23) = happyShift action_7
action_68 (25) = happyShift action_8
action_68 (27) = happyShift action_9
action_68 (28) = happyShift action_10
action_68 (29) = happyShift action_11
action_68 (33) = happyShift action_12
action_68 (34) = happyShift action_13
action_68 (35) = happyShift action_14
action_68 (36) = happyShift action_15
action_68 (11) = happyGoto action_73
action_68 _ = happyFail

action_69 _ = happyReduce_3

action_70 (14) = happyShift action_3
action_70 (5) = happyGoto action_72
action_70 _ = happyFail

action_71 _ = happyReduce_17

action_72 _ = happyReduce_4

action_73 (14) = happyShift action_31
action_73 (17) = happyShift action_32
action_73 (23) = happyShift action_33
action_73 _ = happyReduce_25

action_74 _ = happyReduce_8

action_75 _ = happyReduce_6

action_76 (14) = happyShift action_5
action_76 (19) = happyShift action_6
action_76 (23) = happyShift action_7
action_76 (25) = happyShift action_8
action_76 (27) = happyShift action_9
action_76 (28) = happyShift action_10
action_76 (29) = happyShift action_11
action_76 (33) = happyShift action_12
action_76 (34) = happyShift action_13
action_76 (35) = happyShift action_14
action_76 (36) = happyShift action_15
action_76 (11) = happyGoto action_77
action_76 _ = happyFail

action_77 (14) = happyShift action_31
action_77 (17) = happyShift action_32
action_77 (23) = happyShift action_33
action_77 _ = happyReduce_9

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn11  happy_var_3)
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
		 ([happy_var_1]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  8 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn8
		 ([happy_var_2]
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  9 happyReduction_9
happyReduction_9 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn9
		 (TAlt happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  10 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn6
		 ([happy_var_2]
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 4 10 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (happy_var_1 ++ [happy_var_3]
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn11
		 (TVar happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn11
		 (TSet
	)

happyReduce_14 = happySpecReduce_2  11 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_2)
	(HappyTerminal (TokenInj happy_var_1))
	 =  HappyAbsSyn11
		 (TInj happy_var_1 happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  11 happyReduction_15
happyReduction_15 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (TApp happy_var_1 (TVar happy_var_2)
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 5 11 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (TCase happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 5 11 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (TInd happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (TPi [TBind Nothing happy_var_1] happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 4 11 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (TApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_3  11 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 11 happyReduction_21
happyReduction_21 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (TLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 4 11 happyReduction_22
happyReduction_22 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (TFix happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 4 11 happyReduction_23
happyReduction_23 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (TAny happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 4 11 happyReduction_24
happyReduction_24 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (TPi happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 6 11 happyReduction_25
happyReduction_25 ((HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (TLet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 4 12 happyReduction_26
happyReduction_26 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_1  13 happyReduction_27
happyReduction_27 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  13 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_28 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 37 37 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 14;
	TokenBar -> cont 15;
	TokenTypeOf -> cont 16;
	TokenArr -> cont 17;
	TokenDArr -> cont 18;
	TokenSet -> cont 19;
	TokenEq -> cont 20;
	TokenOS -> cont 21;
	TokenCS -> cont 22;
	TokenOP -> cont 23;
	TokenCP -> cont 24;
	TokenMatch -> cont 25;
	TokenWith -> cont 26;
	TokenFun -> cont 27;
	TokenFix -> cont 28;
	TokenLet -> cont 29;
	TokenIn -> cont 30;
	TokenType -> cont 31;
	TokenEnd -> cont 32;
	TokenInj happy_dollar_dollar -> cont 33;
	TokenInd -> cont 34;
	TokenAny -> cont 35;
	TokenPi -> cont 36;
	_ -> happyError' (tk:tks)
	}

happyError_ 37 tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

happyTerm tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn11 z -> happyReturn z; _other -> notHappyAtAll })

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

instance Err.Monad m => Env.Readable (ReaderT Scope m) where
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
program = 
    withEmptyScope
  . mapM_ define
  . happyProgram 
  . lexer
  where
  define (name, raw_term) = do
    term <- parseRawTerm raw_term
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

parseRawTerm :: RawTerm -> ParserMonad m Term
parseRawTerm TAbsurd = return Absurd
parseRawTerm TSet = return Set
parseRawTerm (TVar var) = lookupTerm var
parseRawTerm (TInj n rty) = do
  ty <- parseRawTerm rty
  case ty of
    Ind _ cons 
      | fromEnum n >= length cons -> do
          ty_s <- showM ty
          Err.throw 
            $ "Trying to inject into constructor " ++ show n
            ++ " of [" ++ ty_s ++ "] which only has " 
            ++ show (length cons) ++ " constructors."
      | otherwise -> 
          return (Inj n ty)
    _ -> do
      ty_s <- showM ty
      Err.throw 
        $ "The argument type of an inj [" ++ ty_s 
        ++ "] must be an inductive type."
parseRawTerm (TApp rt1 rt2) = do
  t1 <- parseRawTerm rt1 
  t2 <- parseRawTerm rt2
  return (App t1 t2)
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
    cons = unfoldInd ind_ty

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
