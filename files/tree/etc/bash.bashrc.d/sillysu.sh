# -*- Mode: shell-script -*-
# shellcheck shell=bash

function sillysu() {
    if [ -n "$DISPLAY" ] ; then
        command cp -a $XAUTHORITY /tmp/.$USER.Xauthority.tmp  # this temporary file is pulled in by root's login scripts
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
            COMMAND=
	    while (( $# > 0 )); do
		arg=$(printf "%q" "$1")
		COMMAND="${COMMAND} ${arg}"
		shift
	    done

            sudo env PUSER=$USER bash --login -c "${COMMAND}"
        )
    fi
}
complete -F _root_command sillysu
