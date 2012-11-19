EXTS = -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances -XViewPatterns -XTypeFamilies -XBangPatterns -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable -XDoRec -XRankNTypes -XGeneralizedNewtypeDeriving
FLAGS = -funbox-strict-fields -hidir obj -odir obj -isrc -itest $(EXTS)
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

