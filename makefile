EXTS = -XTemplateHaskell -XTypeOperators -XFunctionalDependencies -XGADTs -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances -XViewPatterns -XTypeFamilies -XBangPatterns -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable -XRecursiveDo -XRankNTypes -XGeneralizedNewtypeDeriving -XConstraintKinds
FLAGS = -funbox-strict-fields -hide-package groupoids -hidir obj -odir obj -isrc -itest $(EXTS)
POWER = -O2
NORMAL = -O
MAIN = src/Main.hs

.PHONY : all power ghci clean

all:
	ghc --make -o elea $(NORMAL) $(FLAGS) $(MAIN)

power:
	ghc --make -o elea $(POWER) $(FLAGS) $(MAIN)

ghci:
	ghci -fobject-code $(NORMAL) $(FLAGS) $(MAIN)
	
clean:
	rm -rf obj/

