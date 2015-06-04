default: build

build: Base64.elm Ascii.elm BitList.elm
	./.cabal-sandbox/bin/elm-make --yes Base64.elm --output base64.js

clean-deps:
	rm -rf .cabal-sandbox && rm -rf elm-stuf && rm -f cabal.sandbox.config && rm elm-io.sh

clean:
	rm -f *.js && rm -rf elm-stuff/build-artifacts

deps:
	cabal sandbox init && cabal install -j elm-compiler-0.15 elm-package-0.5 elm-make-0.1.2 && ./.cabal-sandbox/bin/elm-package install --yes && wget -N https://raw.githubusercontent.com/maxsnew/IO/master/elm-io.sh

.PHONY: test

test:
	elm-make Test/Main.elm --output Test/raw-test.js && bash elm-io.sh Test/raw-test.js Test/test.js && node Test/test.js
