# PWYF's build, in one place.
#
#   make          build and bundle (and bump the cache-buster)
#   make check    the tests, then the state machines through Glassbox's lint
#
# `-p producing-with-your-feet` is required: the workspace has a second
# package (pwyf-store), and without it spago says the build succeeded and then
# refuses to bundle, leaving a stale static/index.js behind.

GLASSBOX := ../../purescript-hylograph-libs/purescript-glassbox
MACHINES := $(CURDIR)/static/machines

.PHONY: all build bundle test lint check

all: bundle

build:
	spago build

bundle: build
	spago bundle -p producing-with-your-feet
	sed -i "s/index.js?v=[0-9]*/index.js?v=$$(date +%s)/" static/index.html

test:
	spago test -p producing-with-your-feet

# The same lint that gates the machine at load, run before it ships rather than
# when a page meets it (review 2026-11-05).
lint:
	cd $(GLASSBOX) && spago run -p glassbox-cli --quiet -- lint $(MACHINES)

check: test lint
