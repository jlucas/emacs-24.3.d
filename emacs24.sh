#!/bin/sh

PROG=$(basename $0)

EMACS_BINARY=/usr/bin/emacs

EMACS24_HOME=$(readlink -f $(dirname $0)/..)

if ! [ $($EMACS_BINARY -Q --batch --eval '(print emacs-major-version)') -eq 24 ]
then
	echo "$PROG: error: $EMACS_BINARY is not emacs 24.x. Aborting." >&2
	exit
fi

echo "$PROG: info: Launching Emacs 24 with home: $EMACS24_HOME"

exec /usr/bin/env HOME=$EMACS24_HOME $EMACS_BINARY "$@"

