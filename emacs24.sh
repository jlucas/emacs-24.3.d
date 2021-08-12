#!/bin/sh

PROG=$(basename $0)

EMACS_BINARY=/usr/bin/emacs

TEMPHOME=$(readlink -f $(dirname $0)/..)

if ! [ $($EMACS_BINARY -Q --batch --eval '(print emacs-major-version)') -eq 24 ]
then
	echo "$PROG: error: $EMACS_BINARY is not emacs 24.x. Aborting." >&2
	exit
fi

echo "$PROG: info: Launching Emacs 24 with home: $TEMPHOME"

exec /usr/bin/env HOME=$TEMPHOME $EMACS_BINARY "$@"

