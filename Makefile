default: test

clean:
	rm -rf elm-stuff/build-artifacts

.PHONY: test

test:
	elm-test TestRunner.elm

publish:
	elm-package publish
