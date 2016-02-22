EXTS = -XImplicitParams -XStandaloneDeriving -XNoImplicitPrelude -XTemplateHaskell -XTypeOperators -XFunctionalDependencies -XGADTs -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances -XViewPatterns -XTypeFamilies -XBangPatterns -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable -XRecursiveDo -XRankNTypes -XGeneralizedNewtypeDeriving -XConstraintKinds
FLAGS = -package ghc -cpp -hidir obj -odir obj -isrc -itest 
MAIN = src/Main.hs

.PHONY : ghci happy clean

ghci:
	ghci -fobject-code -O -DTRACE -DASSERT $(FLAGS) $(EXTS) $(MAIN)

power:
	cabal configure --enable-optimization
	cabal build
	xcopy /y /F .\dist\build\elea\elea.exe .\elea.exe

trace: 
	cabal configure --enable-optimization --ghc-options="-DTRACE -DASSERT" --builddir=dist-trace
	cabal build --builddir=dist-trace
	xcopy /y .\dist-trace\build\elea\elea.exe .\elea.exe

debug:
	cabal configure --enable-optimization --ghc-options="-DASSERT -DTRACE"
	cabal build
	xcopy /y .\dist\build\elea\elea.exe .\elea.exe

warn:
	ghc --make -o elea.exe -Wall $(FLAGS) $(EXTS) $(MAIN)

happy:
	happy src/Elea/Parser/Calculus.y -o src/Elea/Parser/Calculus.hs

clean:
	rm -rf obj/
