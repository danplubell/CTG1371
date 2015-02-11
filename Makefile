.PHONY: all bench build clean configure haddock hpc install repl run test

all: install configure build haddock test hpc bench

bench:
	cabal bench --jobs

build:
	cabal build --jobs

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	cabal configure --enable-benchmarks --enable-tests

haddock:
	cabal haddock --hyperlink-source
	# dist/doc/html/CTG1371/index.html

hpc:
	hpc markup --destdir=tmp dist/hpc/tix/tests/tests.tix
	# tmp/hpc_index.html

install:
	cabal sandbox init
	cabal install --enable-benchmarks --enable-tests --jobs --only-dependencies --reorder-goals

repl:
	cabal repl lib:CTG1371

run:
	cabal run --jobs CTG1371

test:
	test -d dist/hpc/tix/tests || mkdir -p dist/hpc/tix/tests
	export HPCTIXFILE=dist/hpc/tix/tests/tests.tix; cabal test
	cabal check
