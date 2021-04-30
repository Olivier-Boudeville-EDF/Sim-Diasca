#!/bin/sh

usage="$(basename $0): a script for the compilation to happen correctly when creating an hex package."

guessed_package_name=$(basename $(pwd))

# See the pre_hooks key in rebar-for-hex.config.

# Not using set -e: http://mywiki.wooledge.org/BashFAQ/105

echo "  Hex compile hook script started: preparing for package '${guessed_package_name}', in $(pwd)..."

# Typically run, for a package foo, from _build/default/lib/foo.

# Securing all files needed for a proper build:

for f in priv/hex-packaging/root/GNUmake* ; do ln -sf $f ; done || exit 5

# For its makefiles that are included from the root ones:
#ln -sf priv/hex-packaging/doc/
/bin/cp -rf priv/hex-packaging/doc/ . || exit 10

# Does mostly for the current package, seen as a dependency, what is done by the
# package itself when creating an OTP application for it (see the
# rebar3-{create-tree, populate-build-tree, etc.} make targets) - except we
# directly work in from the application package root (ex: from
# "_build/default/lib/myriad") rather than from its vanilla source tree:


# Like the rebar3-create-tree target:
mkdir -p doc ebin include priv src test || exit 15

make -s all || exit 20

# Like the rebar3-copy-beams target:
find src -name '*.beam' -a ! -name '*_test.beam' -exec /bin/cp -f '{}' ebin/ ';' || exit 25

find src -name '*.hrl' -exec /bin/cp -f '{}' include/ ';' || exit 30

# Note that only the 'src' directory is available in an hex package; if the test
# sources are among the sources in 'src', they will be found, whereas if there
# is for example a 'tests' directory in the same directory as the 'src' one, it
# will not even be included in the hex package.
#
find src -name '*_test.erl' -exec /bin/cp -f '{}' test/ ';' || exit 35


# OTP conventions:
/bin/cp -f src/${guessed_package_name}.app.src ebin/${guessed_package_name}.app || exit 40

echo "  Successful end of hex compile hook script for ${guessed_package_name}, in $(pwd)"

# Hides the source files, otherwise rebar will attempt to recompile them (reason
# unclear) and fail:
#
# (not needing to hide any source file in 'test')
#
find src -name '*rl' -exec /bin/mv -f '{}' '{}'-hidden ';'
