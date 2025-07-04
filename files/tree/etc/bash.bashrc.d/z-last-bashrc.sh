# -*- Mode: shell-script -*-
# shellcheck shell=bash

# if nothing in PS1 and PROMPT_COMMAND had caused the environment to
# be set up by now, we better go at least set up paths if not the
# interactive stuff

if [ -n "$NONINTERACT" ] || [ -z "$PS1" ] ; then
    # echo running setup_environment in non-interactive environment 1>&2
    setup_environment
    # echo finished set_environment in non-interactive environment... 1>&2
    # else
    # echo finished interactive shell initial environment setup pre-prompt 1>&2
fi
