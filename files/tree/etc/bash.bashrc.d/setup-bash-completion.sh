# -*- Mode: shell-script -*-
# shellcheck shell=bash

function setup_home_bash_completion() {
    BASH_COMPLETION_USER_DIR=~/.bash_completion.d

    # much of this copied directly out of 2013-era Debian
    # /usr/share/bash-completion/bash_completion and modified to add
    # to (instead of substitute for) /etc/bash_completion.d with our
    # own directory

    # Glob for matching various backup files.
    #
    _backup_glob='@(#*#|*@(~|.@(bak|orig|rej|swp|dpkg*|rpm@(orig|new|save))))'

    # source compat completion directory definitions
    if [[ -d $BASH_COMPLETION_USER_DIR && -r $BASH_COMPLETION_USER_DIR && \
              -x $BASH_COMPLETION_USER_DIR ]]; then
        for i in $(LC_ALL=C command ls "$BASH_COMPLETION_USER_DIR"); do
            i=$BASH_COMPLETION_USER_DIR/$i
            [[ ${i##*/} != @($_backup_glob|Makefile*|$_blacklist_glob) \
                   && -f $i && -r $i ]] && . "$i"
        done
    fi
    unset i _blacklist_glob
}
