default: run

run:
	ghci -Wall Example

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
