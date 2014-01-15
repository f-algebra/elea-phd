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
import qualified Elea.Evaluation as Eval
import qualified Elea.Env as Env
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Definitions as Defs      
import qualified Elea.Monad.Error as Err
import qualified Data.Map as Map

-- Inductive data types
type TypeDef = (String, [[String]])

-- Let bindings of terms
type TermDef = (String, RawTerm)

-- "show" declarations to decide equality between terms
type PropDef = (String, [RawBind], (RawTerm, RawTerm))

data RawProgram 
  = RawProgram  { _programTypes :: [TypeDef]
                , _programTerms :: [TermDef]
                , _programProps :: [PropDef] }

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
	| HappyAbsSyn12 (RawBind)
	| HappyAbsSyn13 ([RawBind])
	| HappyAbsSyn14 (RawTerm)
	| HappyAbsSyn15 (TermDef)
	| HappyAbsSyn16 ((RawTerm, RawTerm))
	| HappyAbsSyn17 (PropDef)
	| HappyAbsSyn18 (RawProgram)

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
 action_85 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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

action_0 (35) = happyShift action_19
action_0 (40) = happyShift action_20
action_0 (43) = happyShift action_21
action_0 (8) = happyGoto action_15
action_0 (15) = happyGoto action_16
action_0 (17) = happyGoto action_17
action_0 (18) = happyGoto action_18
action_0 _ = happyFail

action_1 (19) = happyShift action_8
action_1 (28) = happyShift action_9
action_1 (30) = happyShift action_10
action_1 (31) = happyShift action_11
action_1 (33) = happyShift action_12
action_1 (34) = happyShift action_13
action_1 (35) = happyShift action_14
action_1 (14) = happyGoto action_7
action_1 _ = happyFail

action_2 (19) = happyShift action_4
action_2 (28) = happyShift action_6
action_2 (6) = happyGoto action_5
action_2 _ = happyFail

action_3 (19) = happyShift action_4
action_3 _ = happyFail

action_4 _ = happyReduce_3

action_5 (22) = happyShift action_39
action_5 (45) = happyAccept
action_5 _ = happyFail

action_6 (19) = happyShift action_4
action_6 (28) = happyShift action_6
action_6 (6) = happyGoto action_38
action_6 _ = happyFail

action_7 (19) = happyShift action_36
action_7 (28) = happyShift action_37
action_7 (45) = happyAccept
action_7 _ = happyFail

action_8 _ = happyReduce_17

action_9 (19) = happyShift action_8
action_9 (28) = happyShift action_9
action_9 (30) = happyShift action_10
action_9 (31) = happyShift action_11
action_9 (33) = happyShift action_12
action_9 (34) = happyShift action_13
action_9 (35) = happyShift action_14
action_9 (14) = happyGoto action_35
action_9 _ = happyFail

action_10 (19) = happyShift action_4
action_10 (28) = happyShift action_6
action_10 (6) = happyGoto action_34
action_10 _ = happyFail

action_11 (19) = happyShift action_8
action_11 (28) = happyShift action_9
action_11 (30) = happyShift action_10
action_11 (31) = happyShift action_11
action_11 (33) = happyShift action_12
action_11 (34) = happyShift action_13
action_11 (35) = happyShift action_14
action_11 (14) = happyGoto action_33
action_11 _ = happyFail

action_12 (28) = happyShift action_31
action_12 (12) = happyGoto action_29
action_12 (13) = happyGoto action_32
action_12 _ = happyFail

action_13 (28) = happyShift action_31
action_13 (12) = happyGoto action_29
action_13 (13) = happyGoto action_30
action_13 _ = happyFail

action_14 (19) = happyShift action_28
action_14 _ = happyFail

action_15 _ = happyReduce_31

action_16 _ = happyReduce_32

action_17 _ = happyReduce_33

action_18 (35) = happyShift action_19
action_18 (40) = happyShift action_20
action_18 (43) = happyShift action_21
action_18 (45) = happyAccept
action_18 (8) = happyGoto action_25
action_18 (15) = happyGoto action_26
action_18 (17) = happyGoto action_27
action_18 _ = happyFail

action_19 (19) = happyShift action_24
action_19 _ = happyFail

action_20 (19) = happyShift action_23
action_20 _ = happyFail

action_21 (19) = happyShift action_22
action_21 _ = happyFail

action_22 (21) = happyShift action_52
action_22 _ = happyFail

action_23 (25) = happyShift action_51
action_23 _ = happyFail

action_24 (25) = happyShift action_50
action_24 _ = happyFail

action_25 _ = happyReduce_34

action_26 _ = happyReduce_35

action_27 _ = happyReduce_36

action_28 (25) = happyShift action_49
action_28 _ = happyFail

action_29 _ = happyReduce_15

action_30 (22) = happyShift action_48
action_30 (28) = happyShift action_31
action_30 (12) = happyGoto action_45
action_30 _ = happyFail

action_31 (19) = happyShift action_47
action_31 _ = happyFail

action_32 (22) = happyShift action_46
action_32 (28) = happyShift action_31
action_32 (12) = happyGoto action_45
action_32 _ = happyFail

action_33 (19) = happyShift action_36
action_33 (28) = happyShift action_37
action_33 (32) = happyShift action_44
action_33 _ = happyFail

action_34 (22) = happyShift action_39
action_34 _ = happyReduce_18

action_35 (19) = happyShift action_36
action_35 (28) = happyShift action_37
action_35 (29) = happyShift action_43
action_35 _ = happyFail

action_36 _ = happyReduce_19

action_37 (19) = happyShift action_8
action_37 (28) = happyShift action_9
action_37 (30) = happyShift action_10
action_37 (31) = happyShift action_11
action_37 (33) = happyShift action_12
action_37 (34) = happyShift action_13
action_37 (35) = happyShift action_14
action_37 (14) = happyGoto action_42
action_37 _ = happyFail

action_38 (22) = happyShift action_39
action_38 (29) = happyShift action_41
action_38 _ = happyFail

action_39 (19) = happyShift action_4
action_39 (28) = happyShift action_6
action_39 (6) = happyGoto action_40
action_39 _ = happyFail

action_40 (22) = happyShift action_39
action_40 _ = happyReduce_5

action_41 _ = happyReduce_4

action_42 (19) = happyShift action_36
action_42 (28) = happyShift action_37
action_42 (29) = happyShift action_66
action_42 _ = happyFail

action_43 _ = happyReduce_21

action_44 (20) = happyShift action_65
action_44 (10) = happyGoto action_64
action_44 _ = happyFail

action_45 _ = happyReduce_16

action_46 (19) = happyShift action_8
action_46 (28) = happyShift action_9
action_46 (30) = happyShift action_10
action_46 (31) = happyShift action_11
action_46 (33) = happyShift action_12
action_46 (34) = happyShift action_13
action_46 (35) = happyShift action_14
action_46 (14) = happyGoto action_63
action_46 _ = happyFail

action_47 (21) = happyShift action_62
action_47 _ = happyFail

action_48 (19) = happyShift action_8
action_48 (28) = happyShift action_9
action_48 (30) = happyShift action_10
action_48 (31) = happyShift action_11
action_48 (33) = happyShift action_12
action_48 (34) = happyShift action_13
action_48 (35) = happyShift action_14
action_48 (14) = happyGoto action_61
action_48 _ = happyFail

action_49 (19) = happyShift action_8
action_49 (28) = happyShift action_9
action_49 (30) = happyShift action_10
action_49 (31) = happyShift action_11
action_49 (33) = happyShift action_12
action_49 (34) = happyShift action_13
action_49 (35) = happyShift action_14
action_49 (14) = happyGoto action_60
action_49 _ = happyFail

action_50 (19) = happyShift action_8
action_50 (28) = happyShift action_9
action_50 (30) = happyShift action_10
action_50 (31) = happyShift action_11
action_50 (33) = happyShift action_12
action_50 (34) = happyShift action_13
action_50 (35) = happyShift action_14
action_50 (14) = happyGoto action_59
action_50 _ = happyFail

action_51 (19) = happyShift action_58
action_51 (7) = happyGoto action_56
action_51 (9) = happyGoto action_57
action_51 _ = happyFail

action_52 (19) = happyShift action_8
action_52 (28) = happyShift action_9
action_52 (30) = happyShift action_10
action_52 (31) = happyShift action_11
action_52 (33) = happyShift action_12
action_52 (34) = happyShift action_13
action_52 (35) = happyShift action_14
action_52 (44) = happyShift action_55
action_52 (14) = happyGoto action_53
action_52 (16) = happyGoto action_54
action_52 _ = happyFail

action_53 (19) = happyShift action_36
action_53 (25) = happyShift action_76
action_53 (28) = happyShift action_37
action_53 _ = happyReduce_27

action_54 _ = happyReduce_30

action_55 (28) = happyShift action_31
action_55 (12) = happyGoto action_29
action_55 (13) = happyGoto action_75
action_55 _ = happyFail

action_56 (20) = happyShift action_74
action_56 _ = happyReduce_8

action_57 (19) = happyShift action_73
action_57 _ = happyReduce_6

action_58 _ = happyReduce_9

action_59 (19) = happyShift action_36
action_59 (28) = happyShift action_37
action_59 _ = happyReduce_26

action_60 (19) = happyShift action_36
action_60 (28) = happyShift action_37
action_60 (36) = happyShift action_72
action_60 _ = happyFail

action_61 (19) = happyShift action_36
action_61 (28) = happyShift action_37
action_61 _ = happyReduce_23

action_62 (19) = happyShift action_4
action_62 (28) = happyShift action_6
action_62 (6) = happyGoto action_71
action_62 _ = happyFail

action_63 (19) = happyShift action_36
action_63 (28) = happyShift action_37
action_63 _ = happyReduce_22

action_64 (20) = happyShift action_69
action_64 (38) = happyShift action_70
action_64 _ = happyFail

action_65 (19) = happyShift action_58
action_65 (9) = happyGoto action_67
action_65 (11) = happyGoto action_68
action_65 _ = happyFail

action_66 _ = happyReduce_20

action_67 (19) = happyShift action_73
action_67 (22) = happyShift action_83
action_67 _ = happyFail

action_68 _ = happyReduce_11

action_69 (19) = happyShift action_58
action_69 (9) = happyGoto action_67
action_69 (11) = happyGoto action_82
action_69 _ = happyFail

action_70 _ = happyReduce_25

action_71 (22) = happyShift action_39
action_71 (29) = happyShift action_81
action_71 _ = happyFail

action_72 (19) = happyShift action_8
action_72 (28) = happyShift action_9
action_72 (30) = happyShift action_10
action_72 (31) = happyShift action_11
action_72 (33) = happyShift action_12
action_72 (34) = happyShift action_13
action_72 (35) = happyShift action_14
action_72 (14) = happyGoto action_80
action_72 _ = happyFail

action_73 _ = happyReduce_10

action_74 (19) = happyShift action_58
action_74 (9) = happyGoto action_79
action_74 _ = happyFail

action_75 (22) = happyShift action_78
action_75 (28) = happyShift action_31
action_75 (12) = happyGoto action_45
action_75 _ = happyFail

action_76 (19) = happyShift action_8
action_76 (28) = happyShift action_9
action_76 (30) = happyShift action_10
action_76 (31) = happyShift action_11
action_76 (33) = happyShift action_12
action_76 (34) = happyShift action_13
action_76 (35) = happyShift action_14
action_76 (14) = happyGoto action_77
action_76 _ = happyFail

action_77 (19) = happyShift action_36
action_77 (28) = happyShift action_37
action_77 _ = happyReduce_28

action_78 (19) = happyShift action_8
action_78 (28) = happyShift action_9
action_78 (30) = happyShift action_10
action_78 (31) = happyShift action_11
action_78 (33) = happyShift action_12
action_78 (34) = happyShift action_13
action_78 (35) = happyShift action_14
action_78 (14) = happyGoto action_53
action_78 (16) = happyGoto action_85
action_78 _ = happyFail

action_79 (19) = happyShift action_73
action_79 _ = happyReduce_7

action_80 (19) = happyShift action_36
action_80 (28) = happyShift action_37
action_80 _ = happyReduce_24

action_81 _ = happyReduce_14

action_82 _ = happyReduce_12

action_83 (19) = happyShift action_8
action_83 (28) = happyShift action_9
action_83 (30) = happyShift action_10
action_83 (31) = happyShift action_11
action_83 (33) = happyShift action_12
action_83 (34) = happyShift action_13
action_83 (35) = happyShift action_14
action_83 (14) = happyGoto action_84
action_83 _ = happyFail

action_84 (19) = happyShift action_36
action_84 (28) = happyShift action_37
action_84 _ = happyReduce_13

action_85 _ = happyReduce_29

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (TokenName happy_var_1))
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
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
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
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TBind happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  13 happyReduction_15
happyReduction_15 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  13 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  14 happyReduction_17
happyReduction_17 (HappyTerminal (TokenName happy_var_1))
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
happyReduction_19 (HappyTerminal (TokenName happy_var_2))
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
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (TLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 4 14 happyReduction_23
happyReduction_23 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
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

happyReduce_25 = happyReduce 5 14 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (TCase happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 4 15 happyReduction_26
happyReduction_26 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn16
		 ((happy_var_1, TVar "True")
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  16 happyReduction_28
happyReduction_28 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn16
		 ((happy_var_1, happy_var_3)
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happyReduce 7 17 happyReduction_29
happyReduction_29 ((HappyAbsSyn16  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((happy_var_2, happy_var_5, happy_var_7)
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 4 17 happyReduction_30
happyReduction_30 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((happy_var_2, [], happy_var_4)
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_1  18 happyReduction_31
happyReduction_31 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn18
		 (RawProgram [happy_var_1] [] []
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  18 happyReduction_32
happyReduction_32 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn18
		 (RawProgram [] [happy_var_1] []
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 (RawProgram [] [] [happy_var_1]
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  18 happyReduction_34
happyReduction_34 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (modify programTypes (++ [happy_var_2]) happy_var_1
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  18 happyReduction_35
happyReduction_35 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (modify programTerms (++ [happy_var_2]) happy_var_1
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  18 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (modify programProps (++ [happy_var_2]) happy_var_1
	)
happyReduction_36 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 45 45 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 19;
	TokenBar -> cont 20;
	TokenTypeOf -> cont 21;
	TokenArr -> cont 22;
	TokenDArr -> cont 23;
	TokenSet -> cont 24;
	TokenEq -> cont 25;
	TokenOS -> cont 26;
	TokenCS -> cont 27;
	TokenOP -> cont 28;
	TokenCP -> cont 29;
	TokenAbsurd -> cont 30;
	TokenMatch -> cont 31;
	TokenWith -> cont 32;
	TokenFun -> cont 33;
	TokenFix -> cont 34;
	TokenLet -> cont 35;
	TokenIn -> cont 36;
	TokenType -> cont 37;
	TokenEnd -> cont 38;
	TokenInj happy_dollar_dollar -> cont 39;
	TokenInd -> cont 40;
	TokenAny -> cont 41;
	TokenPi -> cont 42;
	TokenProp -> cont 43;
	TokenAll -> cont 44;
	_ -> happyError' (tk:tks)
	}

happyError_ 45 tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

happyTerm tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn14 z -> happyReturn z; _other -> notHappyAtAll })

happyType tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


withEmptyScope :: ReaderT Scope m a -> m a
withEmptyScope = flip runReaderT (Scope mempty mempty)

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
    
localDef :: MonadReader Scope m => String -> Term -> m a -> m a
localDef name term = id
  . local 
  $ modify bindMap 
  $ Map.insert name term
  
term :: (Err.Can m, Defs.Has m) => String -> m Term
term = id
  . withEmptyScope 
  . parseAndCheckTerm 
  . happyTerm 
  . lexer
  
_type :: (Err.Can m, Defs.Has m) => String -> m Type
_type = id
  . withEmptyScope
  . parseRawType
  . happyType 
  . lexer
  
program :: forall m . (Err.Can m, Defs.Has m) => String -> m [Equation]
program text = 
  withEmptyScope $ do
    mapM_ defineType types
    mapM_ defineTerm terms
    mapM parseProp props
  where
  RawProgram types terms props = happyProgram (lexer text)
  
  parseProp :: PropDef -> ParserMonad m Equation
  parseProp (name, rbs, (rt1, rt2)) = do
    bs <- mapM parseRawBind rbs
    t1 <- Env.bindMany bs (parseAndCheckTerm rt1)
    t2 <- Env.bindMany bs (parseAndCheckTerm rt2)
    return (Equals name bs t1 t2)
  
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
      name = get Type.boundLabel (cons !! n)
  
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
    $ Fix mempty (head bs) 
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
  | TokenProp
  | TokenAll
  
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
lexer ('\"':cs) = TokenName name : lexer rest
  where
  (name, '\"':rest) = span (not . (== '\"')) cs
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
      ("prop", rest) -> TokenProp : lexer rest
      ("forall", rest) -> TokenAll : lexer rest
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
  show TokenProp = "prop"
  show TokenAll = "forall"
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
