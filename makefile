EXTS = -XStandaloneDeriving -XNoImplicitPrelude -XTemplateHaskell -XTypeOperators -XFunctionalDependencies -XGADTs -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances -XViewPatterns -XTypeFamilies -XBangPatterns -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable -XRecursiveDo -XRankNTypes -XGeneralizedNewtypeDeriving -XConstraintKinds
FLAGS = -package ghc -funbox-strict-fields -cpp -hide-package groupoids -hidir obj -odir obj -isrc -itest 
MAIN = src/Main.hs

.PHONY : ghci happy clean

ghci:
	ghci -fobject-code -D__TRACE__ -D__CHECK__ $(FLAGS) $(EXTS) $(MAIN)

power:
	ghc --make -o elea.exe -O $(FLAGS) $(EXTS) $(MAIN)

warn:
	ghc --make -o elea.exe -Wall $(FLAGS) $(EXTS) $(MAIN)

happy:
	happy src/Elea/Parser/Calculus.y -o src/Elea/Parser/Calculus.hs

clean:
	rm -rf obj/
