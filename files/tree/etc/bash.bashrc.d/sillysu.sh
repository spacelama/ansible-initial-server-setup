# -*- Mode: shell-script -*-
# shellcheck shell=bash

function sillysu() {
    if [ -n "$DISPLAY" ] ; then
        command cp -a $XAUTHORITY /tmp/.Xauthority.$USER.tmp  # this temporary file is pulled in by root's login scripts
    fi
    if [ "$#" -eq 0 ] ; then
        (
            unset BASH_PROFILE_EXECUTED PROFILE_EXECUTED BASHRC_EXECUTED http_proxy HTTP_PROXY ftp_proxy WWW_http_GATEWAY no_proxy
            sudo env PUSER=$USER bash --login    #we want to be able to run as login shell, so .bash_logout will run.
        )
    elif [[ "$1" == -u ]] ; then
        shift
        (
            unset BASH_PROFILE_EXECUTED PROFILE_EXECUTED BASHRC_EXECUTED http_proxy HTTP_PROXY ftp_proxy WWW_http_GATEWAY no_proxy
            sudo "$@"
        )
    elif [[ "$1" == -* ]] ; then
        (
            unset BASH_PROFILE_EXECUTED PROFILE_EXECUTED BASHRC_EXECUTED http_proxy HTTP_PROXY ftp_proxy WWW_http_GATEWAY no_proxy
            sudo "$@"
        )
    else
        (
            unset BASH_PROFILE_EXECUTED PROFILE_EXECUTED BASHRC_EXECUTED http_proxy HTTP_PROXY ftp_proxy WWW_http_GATEWAY no_proxy
            # command - quote
            # no need to make COMMAND an array - bash will merge it anyway
            COMMAND="$1" ; shift
	    for arg in "$@"; do
		arg=$(printf "%q" "$arg")
		COMMAND="${COMMAND} ${arg}"
	    done

            sudo env PUSER=$USER bash --login -c "${COMMAND}"
        )
    fi
}
complete -F _root_command sillysu
