EXTS = -XStandaloneDeriving -XNoImplicitPrelude -XTemplateHaskell -XTypeOperators -XFunctionalDependencies -XGADTs -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances -XViewPatterns -XTypeFamilies -XBangPatterns -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable -XRecursiveDo -XRankNTypes -XGeneralizedNewtypeDeriving -XConstraintKinds
FLAGS = -package ghc -cpp -hidir obj -odir obj -isrc -itest 
MAIN = src/Main.hs

.PHONY : ghci happy clean

ghci:
	ghci -fobject-code -DASSERT $(FLAGS) $(EXTS) $(MAIN)

power:
	cabal configure
	cabal build
	xcopy /y .\dist\build\elea\elea.exe .\elea.exe

debug:
	cabal configure --enable-profiling --ghc-options="-DASSERT -fprof-auto"
	cabal build
	xcopy /y .\dist\build\elea\elea.exe .\elea.exe

warn:
	ghc --make -o elea.exe -Wall $(FLAGS) $(EXTS) $(MAIN)

happy:
	happy src/Elea/Parser/Calculus.y -o src/Elea/Parser/Calculus.hs

clean:
	rm -rf obj/
