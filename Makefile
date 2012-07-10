.PHONY: build all clean conf

all: build

build:
	cabal-dev install

conf:
	cabal-dev configure

clean:
	cabal-dev clean


