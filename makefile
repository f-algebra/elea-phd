EXTS = -XTemplateHaskell -XTypeOperators -XFunctionalDependencies -XGADTs -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances -XViewPatterns -XTypeFamilies -XBangPatterns -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable -XRecursiveDo -XRankNTypes -XGeneralizedNewtypeDeriving -XConstraintKinds
FLAGS = -funbox-strict-fields -hide-package groupoids -hidir obj -odir obj -isrc-lib -isrc-exec -itest 
MAIN = src-exec/Main.hs

.PHONY : ghci clean

ghci:
	ghci -fobject-code -O $(FLAGS) $(EXTS) $(MAIN)
	
clean:
	rm -rf obj/

