default:
	# Use either "make th" or "make generic"

th:
	ghci -Wall ExampleTH

generic:
	ghci -Wall ExampleGeneric

clean:
	cabal clean

configure:
	cabal configure

docs: configure
	cabal haddock

install:
	cabal install

opendocs: docs
	open dist/doc/html/Piso/index.html
