#!/bin/sh

set -e

USAGE="Usage: `basename $0` (get|pull|install|diff|record|revert|push|pushall|send|clean|whatsnew|changes|haddock|release|removedist) [pkg] .."

[ $# -lt 1 ] && echo $USAGE && exit 1

DIR=$PWD/`dirname $0`
CMD=$1

shift

repos_action () {
    PKG=$1
    echo == $PKG ==
    if [ ! -d "$DIR/../$PKG" -a ! $CMD = 'get' ]; then
	echo "$PKG is missing."
	echo "Please run \"`basename $0` get $PKG\" first."
	exit 1
    fi
    if [ ! $CMD = 'get' ]; then
	cd $DIR/../$PKG
    else
	cd $DIR/..
    fi
    case $CMD in
	clean) cabal clean
	    ;;
	install) cabal install
	    ;;
	diff) darcs diff -u
	    ;;
	get) [ -d "$PKG" ] || darcs get http://patch-tag.com/r/AndyStewart/$PKG
	    ;;
	pull) darcs pull
	    ;;
	push) darcs push AndyStewart@patch-tag.com:/r/AndyStewart/$PKG  --set-default
	    ;;
	pushall) darcs push AndyStewart@patch-tag.com:/r/AndyStewart/$PKG -a --set-default
	    ;;
	record) darcs record -l --delete-logfile --skip-long-comment
	    ;;
	revert) darcs revert
	    ;;
	send) darcs send -O
	    ;;
	whatsnew) darcs whatsnew -s
	    ;;
    changes) darcs changes --last 10
        ;;
    haddock) cabal configure --user && cabal build && cabal haddock
        ;;
    release) dist-upload
        ;;
    removedist) darcs setpref boringfile ./_darcs/prefs/boring && darcs record -a -m "UpdateBoringFile"
        ;;
	*) echo "Unknown command: $CMD"; echo $USAGE; exit 1
	    ;;
    esac
}

for pkg in ${*:-manatee-core manatee-browser manatee-editor manatee-filemanager manatee-pdfviewer manatee-mplayer manatee-processmanager manatee-imageviewer manatee-terminal manatee-template manatee-ircclient manatee-reader manatee-curl manatee-welcome manatee manatee-all};
do
  repos_action $pkg
done
