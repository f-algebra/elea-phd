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
import qualified Elea.Type as Type
import qualified Elea.Term as Term
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Error as Err
import qualified Data.Map as Map

data RawProgram 
  = Program { _programTerms :: [TermDef] 
            , _programTypes :: [TypeDef] }

type TypeDef = (RawBind, [RawBind])
type TermDef = (String, RawTerm)

data RawBind 
  = TBind { _rawLabel :: Maybe String
          , _rawType :: RawType }
  
data RawType
  = TyVar String
  | TyApp RawType RawType
  | TyFun RawBind RawType
  | TySet

data RawTerm
  = TVar String
  | TApp RawTerm RawTerm
  | TFix [RawBind] RawTerm
  | TLam [RawBind] RawTerm
  | TType RawType
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
	| HappyAbsSyn6 (RawType)
	| HappyAbsSyn7 (TypeDef)
	| HappyAbsSyn8 ([RawBind])
	| HappyAbsSyn9 ([String])
	| HappyAbsSyn10 ([RawAlt])
	| HappyAbsSyn11 (RawAlt)
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
 action_83 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_34 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (31) = happyShift action_15
action_0 (33) = happyShift action_16
action_0 (7) = happyGoto action_12
action_0 (14) = happyGoto action_13
action_0 (15) = happyGoto action_14
action_0 _ = happyFail

action_1 (16) = happyShift action_5
action_1 (23) = happyShift action_6
action_1 (25) = happyShift action_7
action_1 (27) = happyShift action_8
action_1 (29) = happyShift action_9
action_1 (30) = happyShift action_10
action_1 (31) = happyShift action_11
action_1 (13) = happyGoto action_4
action_1 _ = happyFail

action_2 (16) = happyShift action_3
action_2 _ = happyFail

action_3 (18) = happyShift action_33
action_3 _ = happyFail

action_4 (16) = happyShift action_31
action_4 (25) = happyShift action_32
action_4 (34) = happyAccept
action_4 _ = happyFail

action_5 _ = happyReduce_21

action_6 (16) = happyShift action_28
action_6 (21) = happyShift action_29
action_6 (25) = happyShift action_30
action_6 (6) = happyGoto action_27
action_6 _ = happyFail

action_7 (16) = happyShift action_5
action_7 (23) = happyShift action_6
action_7 (25) = happyShift action_7
action_7 (27) = happyShift action_8
action_7 (29) = happyShift action_9
action_7 (30) = happyShift action_10
action_7 (31) = happyShift action_11
action_7 (13) = happyGoto action_26
action_7 _ = happyFail

action_8 (16) = happyShift action_5
action_8 (23) = happyShift action_6
action_8 (25) = happyShift action_7
action_8 (27) = happyShift action_8
action_8 (29) = happyShift action_9
action_8 (30) = happyShift action_10
action_8 (31) = happyShift action_11
action_8 (13) = happyGoto action_25
action_8 _ = happyFail

action_9 (25) = happyShift action_23
action_9 (12) = happyGoto action_24
action_9 _ = happyFail

action_10 (25) = happyShift action_23
action_10 (12) = happyGoto action_22
action_10 _ = happyFail

action_11 (16) = happyShift action_21
action_11 _ = happyFail

action_12 _ = happyReduce_31

action_13 _ = happyReduce_32

action_14 (31) = happyShift action_15
action_14 (33) = happyShift action_16
action_14 (34) = happyAccept
action_14 (7) = happyGoto action_19
action_14 (14) = happyGoto action_20
action_14 _ = happyFail

action_15 (16) = happyShift action_18
action_15 _ = happyFail

action_16 (16) = happyShift action_3
action_16 (5) = happyGoto action_17
action_16 _ = happyFail

action_17 (22) = happyShift action_51
action_17 _ = happyFail

action_18 (22) = happyShift action_50
action_18 _ = happyFail

action_19 _ = happyReduce_33

action_20 _ = happyReduce_34

action_21 (22) = happyShift action_49
action_21 _ = happyFail

action_22 (20) = happyShift action_48
action_22 (25) = happyShift action_46
action_22 _ = happyFail

action_23 (16) = happyShift action_3
action_23 (5) = happyGoto action_47
action_23 _ = happyFail

action_24 (20) = happyShift action_45
action_24 (25) = happyShift action_46
action_24 _ = happyFail

action_25 (16) = happyShift action_31
action_25 (25) = happyShift action_32
action_25 (28) = happyShift action_44
action_25 _ = happyFail

action_26 (16) = happyShift action_31
action_26 (25) = happyShift action_32
action_26 (26) = happyShift action_43
action_26 _ = happyFail

action_27 (16) = happyShift action_39
action_27 (20) = happyShift action_40
action_27 (24) = happyShift action_41
action_27 (25) = happyShift action_42
action_27 _ = happyFail

action_28 _ = happyReduce_3

action_29 _ = happyReduce_4

action_30 (16) = happyShift action_38
action_30 (21) = happyShift action_29
action_30 (25) = happyShift action_30
action_30 (5) = happyGoto action_36
action_30 (6) = happyGoto action_37
action_30 _ = happyFail

action_31 _ = happyReduce_22

action_32 (16) = happyShift action_5
action_32 (23) = happyShift action_6
action_32 (25) = happyShift action_7
action_32 (27) = happyShift action_8
action_32 (29) = happyShift action_9
action_32 (30) = happyShift action_10
action_32 (31) = happyShift action_11
action_32 (13) = happyGoto action_35
action_32 _ = happyFail

action_33 (16) = happyShift action_28
action_33 (21) = happyShift action_29
action_33 (25) = happyShift action_30
action_33 (6) = happyGoto action_34
action_33 _ = happyFail

action_34 (16) = happyShift action_39
action_34 (20) = happyShift action_40
action_34 (25) = happyShift action_42
action_34 _ = happyReduce_2

action_35 (16) = happyShift action_31
action_35 (25) = happyShift action_32
action_35 (26) = happyShift action_69
action_35 _ = happyFail

action_36 (26) = happyShift action_68
action_36 _ = happyFail

action_37 (16) = happyShift action_39
action_37 (20) = happyShift action_40
action_37 (25) = happyShift action_42
action_37 (26) = happyShift action_67
action_37 _ = happyFail

action_38 (18) = happyShift action_33
action_38 _ = happyReduce_3

action_39 _ = happyReduce_5

action_40 (16) = happyShift action_28
action_40 (21) = happyShift action_29
action_40 (25) = happyShift action_30
action_40 (6) = happyGoto action_66
action_40 _ = happyFail

action_41 _ = happyReduce_25

action_42 (16) = happyShift action_28
action_42 (21) = happyShift action_29
action_42 (25) = happyShift action_30
action_42 (6) = happyGoto action_65
action_42 _ = happyFail

action_43 _ = happyReduce_24

action_44 (16) = happyShift action_63
action_44 (17) = happyShift action_64
action_44 (9) = happyGoto action_60
action_44 (10) = happyGoto action_61
action_44 (11) = happyGoto action_62
action_44 _ = happyFail

action_45 (16) = happyShift action_5
action_45 (23) = happyShift action_6
action_45 (25) = happyShift action_7
action_45 (27) = happyShift action_8
action_45 (29) = happyShift action_9
action_45 (30) = happyShift action_10
action_45 (31) = happyShift action_11
action_45 (13) = happyGoto action_59
action_45 _ = happyFail

action_46 (16) = happyShift action_3
action_46 (5) = happyGoto action_58
action_46 _ = happyFail

action_47 (26) = happyShift action_57
action_47 _ = happyFail

action_48 (16) = happyShift action_5
action_48 (23) = happyShift action_6
action_48 (25) = happyShift action_7
action_48 (27) = happyShift action_8
action_48 (29) = happyShift action_9
action_48 (30) = happyShift action_10
action_48 (31) = happyShift action_11
action_48 (13) = happyGoto action_56
action_48 _ = happyFail

action_49 (16) = happyShift action_5
action_49 (23) = happyShift action_6
action_49 (25) = happyShift action_7
action_49 (27) = happyShift action_8
action_49 (29) = happyShift action_9
action_49 (30) = happyShift action_10
action_49 (31) = happyShift action_11
action_49 (13) = happyGoto action_55
action_49 _ = happyFail

action_50 (16) = happyShift action_5
action_50 (23) = happyShift action_6
action_50 (25) = happyShift action_7
action_50 (27) = happyShift action_8
action_50 (29) = happyShift action_9
action_50 (30) = happyShift action_10
action_50 (31) = happyShift action_11
action_50 (13) = happyGoto action_54
action_50 _ = happyFail

action_51 (16) = happyShift action_3
action_51 (5) = happyGoto action_52
action_51 (8) = happyGoto action_53
action_51 _ = happyFail

action_52 _ = happyReduce_11

action_53 (17) = happyShift action_78
action_53 _ = happyReduce_10

action_54 (16) = happyShift action_31
action_54 (25) = happyShift action_32
action_54 _ = happyReduce_30

action_55 (16) = happyShift action_31
action_55 (25) = happyShift action_32
action_55 (32) = happyShift action_77
action_55 _ = happyFail

action_56 (16) = happyShift action_31
action_56 (25) = happyShift action_32
action_56 _ = happyReduce_27

action_57 _ = happyReduce_19

action_58 (26) = happyShift action_76
action_58 _ = happyFail

action_59 (16) = happyShift action_31
action_59 (25) = happyShift action_32
action_59 _ = happyReduce_26

action_60 (16) = happyShift action_74
action_60 (20) = happyShift action_75
action_60 _ = happyFail

action_61 (17) = happyShift action_73
action_61 _ = happyReduce_28

action_62 _ = happyReduce_15

action_63 _ = happyReduce_13

action_64 (16) = happyShift action_63
action_64 (9) = happyGoto action_60
action_64 (11) = happyGoto action_72
action_64 _ = happyFail

action_65 (16) = happyShift action_39
action_65 (20) = happyShift action_40
action_65 (25) = happyShift action_42
action_65 (26) = happyShift action_71
action_65 _ = happyFail

action_66 (16) = happyShift action_39
action_66 (20) = happyShift action_40
action_66 (25) = happyShift action_42
action_66 _ = happyReduce_7

action_67 _ = happyReduce_9

action_68 (20) = happyShift action_70
action_68 _ = happyFail

action_69 _ = happyReduce_23

action_70 (16) = happyShift action_28
action_70 (21) = happyShift action_29
action_70 (25) = happyShift action_30
action_70 (6) = happyGoto action_83
action_70 _ = happyFail

action_71 _ = happyReduce_6

action_72 _ = happyReduce_16

action_73 (16) = happyShift action_63
action_73 (9) = happyGoto action_60
action_73 (11) = happyGoto action_82
action_73 _ = happyFail

action_74 _ = happyReduce_14

action_75 (16) = happyShift action_5
action_75 (23) = happyShift action_6
action_75 (25) = happyShift action_7
action_75 (27) = happyShift action_8
action_75 (29) = happyShift action_9
action_75 (30) = happyShift action_10
action_75 (31) = happyShift action_11
action_75 (13) = happyGoto action_81
action_75 _ = happyFail

action_76 _ = happyReduce_20

action_77 (16) = happyShift action_5
action_77 (23) = happyShift action_6
action_77 (25) = happyShift action_7
action_77 (27) = happyShift action_8
action_77 (29) = happyShift action_9
action_77 (30) = happyShift action_10
action_77 (31) = happyShift action_11
action_77 (13) = happyGoto action_80
action_77 _ = happyFail

action_78 (16) = happyShift action_3
action_78 (5) = happyGoto action_79
action_78 _ = happyFail

action_79 _ = happyReduce_12

action_80 (16) = happyShift action_31
action_80 (25) = happyShift action_32
action_80 _ = happyReduce_29

action_81 (16) = happyShift action_31
action_81 (25) = happyShift action_32
action_81 _ = happyReduce_18

action_82 _ = happyReduce_17

action_83 (16) = happyShift action_39
action_83 (20) = happyShift action_40
action_83 (25) = happyShift action_42
action_83 _ = happyReduce_8

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn5
		 (TBind (Just happy_var_1) happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn6
		 (TyVar happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn6
		 (TySet
	)

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (TyApp happy_var_1 (TyVar happy_var_2)
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 4 6 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (TyApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (TyFun (TBind Nothing happy_var_1) happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 5 6 happyReduction_8
happyReduction_8 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (TyFun happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 7 happyReduction_10
happyReduction_10 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  9 happyReduction_14
happyReduction_14 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  10 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

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
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn8
		 ([happy_var_2]
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 12 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (happy_var_1 ++ [happy_var_3]
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  13 happyReduction_21
happyReduction_21 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn13
		 (TVar happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  13 happyReduction_22
happyReduction_22 (HappyTerminal (TokenName happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (TApp happy_var_1 (TVar happy_var_2)
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 4 13 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  13 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  13 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (TType happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 4 13 happyReduction_26
happyReduction_26 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 4 13 happyReduction_27
happyReduction_27 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TFix happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 4 13 happyReduction_28
happyReduction_28 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TCase happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 6 13 happyReduction_29
happyReduction_29 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (TLet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 4 14 happyReduction_30
happyReduction_30 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_1  15 happyReduction_31
happyReduction_31 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn15
		 (Program [] [happy_var_1]
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 (Program [happy_var_1] []
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  15 happyReduction_33
happyReduction_33 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (modify programTypes (++ [happy_var_2]) happy_var_1
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  15 happyReduction_34
happyReduction_34 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (modify programTerms (++ [happy_var_2]) happy_var_1
	)
happyReduction_34 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 34 34 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 16;
	TokenBar -> cont 17;
	TokenType -> cont 18;
	TokenEnd -> cont 19;
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
	TokenRec -> cont 30;
	TokenLet -> cont 31;
	TokenIn -> cont 32;
	TokenTypeDef -> cont 33;
	_ -> happyError' (tk:tks)
	}

happyError_ 34 tk tks = happyError' tks
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

instance Monad m => Type.Env (ReaderT Scope m) where
  bindAt at b = 
      local
    $ modify bindMap (map (liftAt at) . addToMap)
    . modify bindStack (insertAt (convertEnum at) b)
    where
    addToMap 
      | Type.Bind (Just lbl) ty <- b = 
          Map.insert lbl (Right (at, ty))
      | otherwise = id

instance Monad m => Type.ReadableEnv (ReaderT Scope m) where
  boundAt at = 
      asks
    $ (!! fromEnum at) . get bindStack
  
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
  
  addTypeDef (rb, r_cons) = do
    b@(Type.Bind (Just name) _) <- parseRawBind rb
    cons <- Type.bind b (mapM parseRawBind r_cons)
    Defs.defineType name (Type.Ind b cons)
  
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
parseRawTerm (TVar var) = lookupTerm var
parseRawTerm TAbsurd = return Term.Absurd
parseRawTerm (TType rty) =
  liftM Term.Type (parseRawType rty)
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
  ind_ty <- Term.typeOf t
  alts <- mapM parseRawAlt ralts
  return (Term.Case t ind_ty alts)
  where
  parseRawAlt (TAlt (con_lbl:var_lbls) rt) = do
    -- Lookup the first label as a var to get the underlying constructor
    con@(Term.Inj {}) <- parseRawTerm (TVar con_lbl)
    -- Use this to get the type of the constructor arguments
    inj_ty <- Term.typeOf con
    let inj_bs = take (length var_lbls) 
               $ get fst
               $ Type.flattenFun inj_ty
    -- Use these types to construct 'Bind's for the bound arguments
    let var_bs = zipWith (set Type.boundLabel . Just) var_lbls inj_bs
    t <- Type.bindMany var_bs (parseRawTerm rt)
    return (Term.Alt var_bs t)
    
parseRawType :: RawType -> ParserMonad m Type
parseRawType (TyVar var) = lookupType var
parseRawType TySet = return Type.Set
parseRawType (TyApp rt1 rt2) = do
  t1 <- parseRawType rt1
  t2 <- parseRawType rt2
  return (Type.App t1 t2)
parseRawType (TyFun rb rty) = do
  b <- parseRawBind rb
  ty <- Type.bind b (parseRawType rty)
  return (Type.Fun b ty)

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
  | TokenType
  | TokenSet
  | TokenArr
  | TokenEnd
  | TokenEq
  | TokenOS
  | TokenCS
  | TokenOP
  | TokenCP
  | TokenMatch
  | TokenWith
  | TokenFun
  | TokenRec
  | TokenLet
  | TokenIn
  | TokenTypeDef
  
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
lexer (':':cs) = TokenType : lexer cs
lexer (';':cs) = TokenEnd : lexer cs
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
      ("fun", rest) -> TokenFun : lexer rest
      ("match", rest) -> TokenMatch : lexer rest
      ("with", rest) -> TokenWith : lexer rest
      ("let", rest) -> TokenLet : lexer rest
      ("fix", rest) -> TokenRec : lexer rest
      ("in", rest) -> TokenIn : lexer rest
      ("type", rest) -> TokenTypeDef : lexer rest
      (name, rest) -> TokenName name : lexer rest
lexer cs = error $ "Unrecognized symbol " ++ take 1 cs

instance Show Token where
  show TokenBar = "|"
  show (TokenName x) = x
  show TokenType = ":"
  show TokenSet = "set"
  show TokenArr = "->"
  show TokenEnd = ";"
  show TokenEq = " = "
  show TokenOS = "["
  show TokenCS = "]"
  show TokenOP = "("
  show TokenCP = ")"
  show TokenMatch = "match"
  show TokenWith = "with"
  show TokenFun = "fun"
  show TokenRec = "fix"
  show TokenLet = "let"
  show TokenIn = "in"
  show TokenTypeDef = "type"
  
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
