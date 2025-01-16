# -*- Mode: shell-script -*-
# shellcheck shell=bash

if [ -e /etc/turnkey_version ] ; then
    PROMPT_COMMAND=handleprompt
fi
