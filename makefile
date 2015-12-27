EXTS = -XStandaloneDeriving -XNoImplicitPrelude -XTemplateHaskell -XTypeOperators -XFunctionalDependencies -XGADTs -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances -XViewPatterns -XTypeFamilies -XBangPatterns -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable -XRecursiveDo -XRankNTypes -XGeneralizedNewtypeDeriving -XConstraintKinds
FLAGS = -package ghc -funbox-strict-fields -hide-package groupoids -hidir obj -odir obj -isrc -itest 
MAIN = src/Main.hs

.PHONY : ghci happy clean

ghci:
	ghci -fobject-code $(FLAGS) $(EXTS) $(MAIN)

power:
	ghc --make -o elea.exe -O $(FLAGS) $(EXTS) $(MAIN)
	
test:
	ghc --make -O -o elea.exe $(FLAGS) $(EXTS) $(MAIN)
	elea.exe 2>&1

happy:
	happy src/Elea/Parser/Calculus.y -o src/Elea/Parser/Calculus.hs

clean:
	rm -rf obj/
