EXTS = -XStandaloneDeriving -XNoImplicitPrelude -XTemplateHaskell -XTypeOperators -XFunctionalDependencies -XGADTs -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances -XViewPatterns -XTypeFamilies -XBangPatterns -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable -XRecursiveDo -XRankNTypes -XGeneralizedNewtypeDeriving -XConstraintKinds
FLAGS = -package ghc -funbox-strict-fields -hide-package groupoids -hidir obj -odir obj -isrc-lib -isrc-exec -itest 
MAIN = src-exec/Main.hs

.PHONY : ghci happy clean

ghci:
	ghci -fobject-code $(FLAGS) $(EXTS) $(MAIN)

power:
	ghc --make -O2 $(FLAGS) $(EXTS) $(MAIN)
	
test:
	ghc --make -O2 -o elea.exe $(FLAGS) $(EXTS) $(MAIN)
	elea.exe 2>&1

happy:
	happy src-lib/Elea/Parser/Calculus.y -o src-lib/Elea/Parser/Calculus.hs

clean:
	rm -rf obj/

