default: build

build: Ascii.elm
	./.cabal-sandbox/bin/elm-make --yes Ascii.elm --output base64.js

clean-deps:
	rm -rf .cabal-sandbox && rm -rf elm-stuf && rm -f cabal.sandbox.config

clean:
	rm -f *.js && rm -rf elm-stuff/build-artifacts

deps:
	cabal sandbox init && cabal install -j elm-compiler-0.15 elm-package-0.5 elm-make-0.1.2 && ./.cabal-sandbox/bin/elm-package install --yes

.PHONY: test

test:
	elm-make Test/Main.elm --output Test/raw-test.js && bash elm-io.sh Test/raw-test.js Test/test.js && node Test/test.js
