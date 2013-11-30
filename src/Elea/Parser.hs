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
import qualified Elea.Type as Type
import qualified Elea.Typing as Typing
import qualified Elea.Evaluation as Eval
import qualified Elea.Env as Env
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Error as Err
import qualified Data.Map as Map

type TypeDef = (String, [[String]])
type TermDef = (String, RawTerm)
type RawProgram = ([TypeDef], [TermDef])

data RawType 
  = TBase String
  | TFun RawType RawType

data RawBind 
  = TBind { _rawLabel :: String
          , _rawType :: RawType }

data RawTerm
  = TVar String
  | TApp RawTerm RawTerm
  | TFix [RawBind] RawTerm
  | TLam [RawBind] RawTerm
  | TCon Nat RawType
  | TAbsurd RawType
  | TCase RawTerm [RawAlt]
  | TLet String RawTerm RawTerm
  
data RawAlt
  = TAlt [String] RawTerm
  
data Scope 
  = Scope { _bindMap :: Map String Term
          , _bindStack :: [Bind] }
  
mkLabels [''Scope, ''RawBind]

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn5 (RawType)
	| HappyAbsSyn6 ([[String]])
	| HappyAbsSyn7 (TypeDef)
	| HappyAbsSyn8 ([String])
	| HappyAbsSyn9 ([RawAlt])
	| HappyAbsSyn10 (RawAlt)
	| HappyAbsSyn11 (RawBind)
	| HappyAbsSyn12 ([RawBind])
	| HappyAbsSyn13 (RawTerm)
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
 action_70 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_29 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (32) = happyShift action_15
action_0 (37) = happyShift action_16
action_0 (7) = happyGoto action_12
action_0 (14) = happyGoto action_13
action_0 (15) = happyGoto action_14
action_0 _ = happyFail

action_1 (16) = happyShift action_5
action_1 (25) = happyShift action_6
action_1 (27) = happyShift action_7
action_1 (28) = happyShift action_8
action_1 (30) = happyShift action_9
action_1 (31) = happyShift action_10
action_1 (32) = happyShift action_11
action_1 (13) = happyGoto action_4
action_1 _ = happyFail

action_2 (16) = happyShift action_3
action_2 _ = happyFail

action_3 _ = happyReduce_2

action_4 (16) = happyShift action_30
action_4 (25) = happyShift action_31
action_4 (40) = happyAccept
action_4 _ = happyFail

action_5 _ = happyReduce_16

action_6 (16) = happyShift action_5
action_6 (25) = happyShift action_6
action_6 (27) = happyShift action_7
action_6 (28) = happyShift action_8
action_6 (30) = happyShift action_9
action_6 (31) = happyShift action_10
action_6 (32) = happyShift action_11
action_6 (13) = happyGoto action_29
action_6 _ = happyFail

action_7 (16) = happyShift action_3
action_7 (25) = happyShift action_28
action_7 (5) = happyGoto action_27
action_7 _ = happyFail

action_8 (16) = happyShift action_5
action_8 (25) = happyShift action_6
action_8 (27) = happyShift action_7
action_8 (28) = happyShift action_8
action_8 (30) = happyShift action_9
action_8 (31) = happyShift action_10
action_8 (32) = happyShift action_11
action_8 (13) = happyGoto action_26
action_8 _ = happyFail

action_9 (25) = happyShift action_24
action_9 (11) = happyGoto action_22
action_9 (12) = happyGoto action_25
action_9 _ = happyFail

action_10 (25) = happyShift action_24
action_10 (11) = happyGoto action_22
action_10 (12) = happyGoto action_23
action_10 _ = happyFail

action_11 (16) = happyShift action_21
action_11 _ = happyFail

action_12 _ = happyReduce_26

action_13 _ = happyReduce_27

action_14 (32) = happyShift action_15
action_14 (37) = happyShift action_16
action_14 (40) = happyAccept
action_14 (7) = happyGoto action_19
action_14 (14) = happyGoto action_20
action_14 _ = happyFail

action_15 (16) = happyShift action_18
action_15 _ = happyFail

action_16 (16) = happyShift action_17
action_16 _ = happyFail

action_17 (22) = happyShift action_43
action_17 _ = happyFail

action_18 (22) = happyShift action_42
action_18 _ = happyFail

action_19 _ = happyReduce_28

action_20 _ = happyReduce_29

action_21 (22) = happyShift action_41
action_21 _ = happyFail

action_22 _ = happyReduce_14

action_23 (19) = happyShift action_40
action_23 (25) = happyShift action_24
action_23 (11) = happyGoto action_37
action_23 _ = happyFail

action_24 (16) = happyShift action_39
action_24 _ = happyFail

action_25 (19) = happyShift action_38
action_25 (25) = happyShift action_24
action_25 (11) = happyGoto action_37
action_25 _ = happyFail

action_26 (16) = happyShift action_30
action_26 (25) = happyShift action_31
action_26 (29) = happyShift action_36
action_26 _ = happyFail

action_27 (19) = happyShift action_35
action_27 _ = happyReduce_17

action_28 (16) = happyShift action_3
action_28 (25) = happyShift action_28
action_28 (5) = happyGoto action_34
action_28 _ = happyFail

action_29 (16) = happyShift action_30
action_29 (25) = happyShift action_31
action_29 (26) = happyShift action_33
action_29 _ = happyFail

action_30 _ = happyReduce_18

action_31 (16) = happyShift action_5
action_31 (25) = happyShift action_6
action_31 (27) = happyShift action_7
action_31 (28) = happyShift action_8
action_31 (30) = happyShift action_9
action_31 (31) = happyShift action_10
action_31 (32) = happyShift action_11
action_31 (13) = happyGoto action_32
action_31 _ = happyFail

action_32 (16) = happyShift action_30
action_32 (25) = happyShift action_31
action_32 (26) = happyShift action_56
action_32 _ = happyFail

action_33 _ = happyReduce_20

action_34 (19) = happyShift action_35
action_34 (26) = happyShift action_55
action_34 _ = happyFail

action_35 (16) = happyShift action_3
action_35 (25) = happyShift action_28
action_35 (5) = happyGoto action_54
action_35 _ = happyFail

action_36 (17) = happyShift action_53
action_36 (9) = happyGoto action_52
action_36 _ = happyFail

action_37 _ = happyReduce_15

action_38 (16) = happyShift action_5
action_38 (25) = happyShift action_6
action_38 (27) = happyShift action_7
action_38 (28) = happyShift action_8
action_38 (30) = happyShift action_9
action_38 (31) = happyShift action_10
action_38 (32) = happyShift action_11
action_38 (13) = happyGoto action_51
action_38 _ = happyFail

action_39 (18) = happyShift action_50
action_39 _ = happyFail

action_40 (16) = happyShift action_5
action_40 (25) = happyShift action_6
action_40 (27) = happyShift action_7
action_40 (28) = happyShift action_8
action_40 (30) = happyShift action_9
action_40 (31) = happyShift action_10
action_40 (32) = happyShift action_11
action_40 (13) = happyGoto action_49
action_40 _ = happyFail

action_41 (16) = happyShift action_5
action_41 (25) = happyShift action_6
action_41 (27) = happyShift action_7
action_41 (28) = happyShift action_8
action_41 (30) = happyShift action_9
action_41 (31) = happyShift action_10
action_41 (32) = happyShift action_11
action_41 (13) = happyGoto action_48
action_41 _ = happyFail

action_42 (16) = happyShift action_5
action_42 (25) = happyShift action_6
action_42 (27) = happyShift action_7
action_42 (28) = happyShift action_8
action_42 (30) = happyShift action_9
action_42 (31) = happyShift action_10
action_42 (32) = happyShift action_11
action_42 (13) = happyGoto action_47
action_42 _ = happyFail

action_43 (16) = happyShift action_46
action_43 (6) = happyGoto action_44
action_43 (8) = happyGoto action_45
action_43 _ = happyFail

action_44 (17) = happyShift action_64
action_44 _ = happyReduce_7

action_45 (16) = happyShift action_63
action_45 _ = happyReduce_5

action_46 _ = happyReduce_8

action_47 (16) = happyShift action_30
action_47 (25) = happyShift action_31
action_47 _ = happyReduce_25

action_48 (16) = happyShift action_30
action_48 (25) = happyShift action_31
action_48 (33) = happyShift action_62
action_48 _ = happyFail

action_49 (16) = happyShift action_30
action_49 (25) = happyShift action_31
action_49 _ = happyReduce_22

action_50 (16) = happyShift action_3
action_50 (25) = happyShift action_28
action_50 (5) = happyGoto action_61
action_50 _ = happyFail

action_51 (16) = happyShift action_30
action_51 (25) = happyShift action_31
action_51 _ = happyReduce_21

action_52 (17) = happyShift action_59
action_52 (35) = happyShift action_60
action_52 _ = happyFail

action_53 (16) = happyShift action_46
action_53 (8) = happyGoto action_57
action_53 (10) = happyGoto action_58
action_53 _ = happyFail

action_54 (19) = happyShift action_35
action_54 _ = happyReduce_4

action_55 _ = happyReduce_3

action_56 _ = happyReduce_19

action_57 (16) = happyShift action_63
action_57 (19) = happyShift action_69
action_57 _ = happyFail

action_58 _ = happyReduce_10

action_59 (16) = happyShift action_46
action_59 (8) = happyGoto action_57
action_59 (10) = happyGoto action_68
action_59 _ = happyFail

action_60 _ = happyReduce_24

action_61 (19) = happyShift action_35
action_61 (26) = happyShift action_67
action_61 _ = happyFail

action_62 (16) = happyShift action_5
action_62 (25) = happyShift action_6
action_62 (27) = happyShift action_7
action_62 (28) = happyShift action_8
action_62 (30) = happyShift action_9
action_62 (31) = happyShift action_10
action_62 (32) = happyShift action_11
action_62 (13) = happyGoto action_66
action_62 _ = happyFail

action_63 _ = happyReduce_9

action_64 (16) = happyShift action_46
action_64 (8) = happyGoto action_65
action_64 _ = happyFail

action_65 (16) = happyShift action_63
action_65 _ = happyReduce_6

action_66 (16) = happyShift action_30
action_66 (25) = happyShift action_31
action_66 _ = happyReduce_23

action_67 _ = happyReduce_13

action_68 _ = happyReduce_11

action_69 (16) = happyShift action_5
action_69 (25) = happyShift action_6
action_69 (27) = happyShift action_7
action_69 (28) = happyShift action_8
action_69 (30) = happyShift action_9
action_69 (31) = happyShift action_10
action_69 (32) = happyShift action_11
action_69 (13) = happyGoto action_70
action_69 _ = happyFail

action_70 (16) = happyShift action_30
action_70 (25) = happyShift action_31
action_70 _ = happyReduce_12

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn5
		 (TBase happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (TFun happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 4 7 happyReduction_7
happyReduction_7 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

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
happyReduction_12 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn10
		 (TAlt happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 5 11 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (TBind happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  12 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn13
		 (TVar happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  13 happyReduction_17
happyReduction_17 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (TAbsurd happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  13 happyReduction_18
happyReduction_18 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (TApp happy_var_1 (TVar happy_var_2)
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 4 13 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_3  13 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 13 happyReduction_21
happyReduction_21 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 4 13 happyReduction_22
happyReduction_22 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TFix happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 6 13 happyReduction_23
happyReduction_23 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TLet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 5 13 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TCase happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 4 14 happyReduction_25
happyReduction_25 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_1  15 happyReduction_26
happyReduction_26 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn15
		 (([happy_var_1], [])
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  15 happyReduction_27
happyReduction_27 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 (([], [happy_var_1])
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  15 happyReduction_28
happyReduction_28 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (first (++ [happy_var_2]) happy_var_1
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  15 happyReduction_29
happyReduction_29 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (second (++ [happy_var_2]) happy_var_1
	)
happyReduction_29 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 40 40 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 16;
	TokenBar -> cont 17;
	TokenTypeOf -> cont 18;
	TokenArr -> cont 19;
	TokenDArr -> cont 20;
	TokenSet -> cont 21;
	TokenEq -> cont 22;
	TokenOS -> cont 23;
	TokenCS -> cont 24;
	TokenOP -> cont 25;
	TokenCP -> cont 26;
	TokenAbsurd -> cont 27;
	TokenMatch -> cont 28;
	TokenWith -> cont 29;
	TokenFun -> cont 30;
	TokenFix -> cont 31;
	TokenLet -> cont 32;
	TokenIn -> cont 33;
	TokenType -> cont 34;
	TokenEnd -> cont 35;
	TokenInj happy_dollar_dollar -> cont 36;
	TokenInd -> cont 37;
	TokenAny -> cont 38;
	TokenPi -> cont 39;
	_ -> happyError' (tk:tks)
	}

happyError_ 40 tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


withEmptyScope :: ReaderT Scope m a -> m a
withEmptyScope = flip runReaderT (Scope mempty mempty)

instance Monad m => Env.Writable (ReaderT Scope m) where
  bindAt at b = 
      local
    $ modify bindMap (addToMap . map (liftAt at))
    . modify bindStack addToStack
    where
    addToStack = insertAt (enum at) b
    addToMap = Map.insert (boundLabel b) (Var at)

instance Err.Monad m => Env.Readable (ReaderT Scope m) where
  bindings = asks (get bindStack)
  
type ParserMonad m a = (Err.Monad m, Defs.Monad m) => ReaderT Scope m a
    
localDef :: MonadReader Scope m => String -> Term -> m a -> m a
localDef name term = id
  . local 
  $ modify bindMap 
  $ Map.insert name term
  
term :: (Err.Monad m, Defs.Monad m) => String -> m Term
term = id
  . withEmptyScope 
  . parseAndCheckTerm 
  . happyTerm 
  . lexer
  
program :: forall m . (Err.Monad m, Defs.Monad m) => String -> m ()
program text = 
  withEmptyScope $ do
    mapM_ defineType types
    mapM_ defineTerm terms
  where
  (types, terms) = happyProgram (lexer text)
  
  defineType :: TypeDef -> ParserMonad m ()
  defineType (ind_name, raw_cons) = do
    cons <- mapM mkCon raw_cons
    let ind_ty = Type.Ind ind_name cons
    Defs.defineType ind_name (Type.Base ind_ty)
    mapM_ (defCon ind_ty) [0..length cons - 1]
    where
    mkCon :: [String] -> ParserMonad m (String, [Type.ConArg])
    mkCon (con_name:ty_names) = do
      args <- mapM mkArg ty_names
      return (con_name, args)
      where
      mkArg :: String -> ParserMonad m Type.ConArg
      mkArg arg_name
        | arg_name == ind_name = return Type.IndVar
        | otherwise = liftM Type.ConArg (lookupType arg_name)
        
    defCon :: Type.Ind -> Int -> ParserMonad m ()
    defCon ind_ty n =
      Defs.defineTerm name (Con ind_ty (enum n))
      where
      cons = Type.unfold ind_ty
      name = Type.boundLabel (cons !! n)
  
  defineTerm (name, raw_term) = do
    term <- parseAndCheckTerm raw_term
    Defs.defineTerm name term
    
lookupTerm :: String -> ParserMonad m Term
lookupTerm name = do
  mby_bind <- asks (Map.lookup name . get bindMap)
  if isJust mby_bind
  then return (fromJust mby_bind)
  else do
    mby_term <- Defs.lookupTerm name
    if isJust mby_term
    then return (fromJust mby_term)
    else Err.throw $ "Undefined term: " ++ name
    
lookupType :: String -> ParserMonad m Type
lookupType name = do
  mby_ty <- Defs.lookupType name
  if isJust mby_ty
  then return (fromJust mby_ty)
  else Err.throw $ "Undefind type: " ++ name
    
parseAndCheckTerm :: RawTerm -> ParserMonad m Term
parseAndCheckTerm = 
  Err.check Typing.check . parseRawTerm
  
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
parseRawTerm (TVar var) = 
  lookupTerm var
parseRawTerm (TAbsurd rty) = do
  ty <- parseRawType rty
  return (Absurd ty)
parseRawTerm (TApp rt1 rt2) = do
  t1 <- parseRawTerm rt1 
  t2 <- parseRawTerm rt2
  return (Eval.run (App t1 [t2]))
parseRawTerm (TFix rbs rt) = do
  bs <- mapM parseRawBind rbs
  t <- Env.bindMany bs (parseRawTerm rt)
  return 
    $ Fix (head bs) 
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
  ind_ty <- Typing.typeOf t
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
    mby_this_con = find ((== con_lbl) . boundLabel) cons
    Just this_con = mby_this_con
    
    -- The bindings for this constructor are the arguments for the type
    con_tys = (init . Type.flatten . boundType) this_con
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
  | TokenAny
  | TokenPi
  
happyError :: [Token] -> a
happyError tokens = error $ "Parse error\n" ++ (show tokens)

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c `elem` "'_[]"
  
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
lexer ('|':cs) = TokenBar : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('{':cs) = TokenName ("{" ++ name ++ "}") : lexer rest
  where
  (name, '}':rest) = span (not . (== '}')) cs
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
      ("type", rest) -> TokenType : lexer rest
      ("ind", rest) -> TokenInd : lexer rest
      ("end", rest) -> TokenEnd : lexer rest
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
