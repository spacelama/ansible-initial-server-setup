# -*- Mode: shell-script -*-
# shellcheck shell=bash

function sh_interactive () {
    case "$-" in
       *i*)
          [ dumb != "$TERM" ] && [ -t 0 ]
          return
       ;;
       *)
          return 1
       ;;
    esac
}

#echo 1 1>&2
if ! sh_interactive ; then
#echo 2 1>&2
    # PS1='$'
    # FIXME: We deliberately don't check for an unset PS1 and do it
    # this way, but why...?  Historical SuSE systems that sucked?
    # Silly hacks I was doing with remote systems?  Anyway, works for now...
    NONINTERACT=1
elif [ -z "$PBS_ENVIRONMENT" ] ; then
    case $TERM in
        xterm*)
            function winname() {
                if [ -t 2 ] ; then
                    echo -ne  '\033]2;'"$1"'\007' 1>&2
                fi
            }
            function iconname() {
                if [ -t 2 ] ; then
                    echo -ne  '\033]1;'"$1"'\007' 1>&2
                fi
            }
            ;;
        *)
            function winname() {
                :
            }
            function iconname() {
                :
            }
            ;;
    esac
    ( winname "Wait... logged into $HOSTNAME and sourcing startup" & )
fi
