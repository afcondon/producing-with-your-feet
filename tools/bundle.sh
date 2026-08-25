#!/usr/bin/env bash
# Bundle the app and stamp the script tag with the bundle's own mtime.
#
# `<script src="./index.js">` with no query is a silent trap: a reload can serve
# yesterday's app, everything looks fine, and the bug you just fixed is still
# there. It has cost this project whole afternoons — the same family as MC6
# writes needing a focused, fresh tab.
#
# The stamp is the bundle's modification time, so it changes exactly when the
# bundle does and never otherwise.
#
# `stat` is deliberately not used: this box has GNU coreutils ahead of BSD stat
# on PATH, so `stat -f %m` reads as "describe the filesystem" and silently
# returns something that is not a timestamp. perl's `stat` is the same
# everywhere.
set -euo pipefail
cd "$(dirname "$0")/.."
spago bundle -p producing-with-your-feet --outfile static/index.js
stamp=$(perl -e 'print((stat("static/index.js"))[9])')
perl -pi -e "s{(<script src=\"\./index\.js)(\?v=\d+)?(\")}{\$1?v=${stamp}\$3}" static/index.html
echo "bundled and stamped v=${stamp}"
