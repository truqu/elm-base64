default: test

clean:
	rm -rf elm-stuff/build-artifacts

.PHONY: test check

test:
	elm-test TestRunner.elm

check:
	elm-make Check/Base64Check.elm --output Check/raw-check.js
	bash elm-io.sh Check/raw-check.js Check/check.js
	node Check/check.js

publish:
	elm-package publish
