EXTS = -XStandaloneDeriving -XNoImplicitPrelude -XTemplateHaskell -XTypeOperators -XFunctionalDependencies -XGADTs -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances -XViewPatterns -XTypeFamilies -XBangPatterns -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable -XRecursiveDo -XRankNTypes -XGeneralizedNewtypeDeriving -XConstraintKinds
FLAGS = -package ghc -funbox-strict-fields -cpp -hidir obj -odir obj -isrc -itest 
MAIN = src/Main.hs

.PHONY : ghci happy clean

ghci:
	ghci -fobject-code -D__TRACE__ -D__CHECK__ $(FLAGS) $(EXTS) $(MAIN)

power:
	ghc --make -o elea.exe -O $(FLAGS) $(EXTS) $(MAIN)

debug:
	cabal configure --enable-profiling --ghc-options="-D__CHECK__ -fprof-auto"
	cabal build
	xcopy /y .\dist\build\elea\elea.exe .\elea-debug.exe

warn:
	ghc --make -o elea.exe -Wall $(FLAGS) $(EXTS) $(MAIN)

happy:
	happy src/Elea/Parser/Calculus.y -o src/Elea/Parser/Calculus.hs

clean:
	rm -rf obj/
