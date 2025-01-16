# -*- Mode: shell-script -*-
# shellcheck shell=bash

# some aliases are always loaded regardless of what kind of login session
alias l='ls -lA'

function setup_aliases() {
    if ls --version 2>/dev/null | grep -E 'coreutils|fileutils' >/dev/null ; then
        export enablecolor="--color=auto"   #now enabled for everything, since darwin now has gnu fileutils
    fi
    export forceenablecolor="--color"

    alias ncal='ncal'
    alias cal='ncal -b'
    alias cal3='ncal -3'
    alias read='read -e' # use readline when interactive anyway
    alias which=: ; unalias which   #you can't unalias something that isn't aliased, and only some systems are stupid enough to alias which
    alias slrn='slrn --kill-log $HOME/News/.kill-log'
    alias ouci=pico   #so sue me
    #    alias pico=nano # ~/bin/pico is a script for emacsclient -nw these days
    alias grep='grep $enablecolor'
    alias ip='ip -color=auto'
    alias pin='echo "did you want pine?"'
    alias ls-tr='ls -lAtr $enablecolor'
    alias lsl='ls -lAL'
    alias d='ls -F $forceenablecolor'
    alias l.='ls -dl .[a-zA-Z]*'
    alias lc='ls -lA $forceenablecolor'
    alias l-tr='ls -lAtr'
    alias lc-tr='ls -lAtr $forceenablecolor'
    alias ll='ls -lA'
    alias ls='ls $enablecolor'
    alias l-ft='ls -lA --time-style=full-iso'
    alias l-lt='ls -lA --time-style=long-iso'
    # might want to also consider ls --quoting-style (-N) or QUOTING_STYLE=literal
    alias p='pstree -ap tconnors'
    alias pavucontrol='DBUS_SESSION_BUS_ADDRESS= pavucontrol'  # https://gitlab.freedesktop.org/pulseaudio/pavucontrol/-/issues/75#note_1467143
    alias bc='bc -l'
    # shellcheck disable=SC2290   # incorrect interpretation of what the code is successfully doing: setting aliases to the "-" and "+" commands
    alias -- +='pushd .'
    # shellcheck disable=SC2290   # incorrect interpretation of what the code is successfully doing: setting aliases to the "-" and "+" commands
    alias -- -='popd'
    alias bd='popd'
    alias ..='cd ..'
    alias ...='cd ../..'
    alias ....='cd ../../..'
    alias .....='cd ../../../..'
    alias ......='cd ../../../../..'
    alias rehash='hash -r'
    alias crontab='crontab -i'
    alias ecrontab='crontab -e'
    alias info=pinfo
    alias c='yes "" | head -n28'

    #https://stackoverflow.com/questions/15292391/is-it-possible-to-perform-a-grep-search-in-all-the-branches-of-a-git-project
    alias grep_git_all_branches="git branch -a | tr -d \* | sed '/->/d' | xargs git grep"
    alias grep_git_all_commits="git rev-list --all | tr -d \* | sed '/->/d' | xargs git grep"
    alias grep_git_show_commits="git log -p --all -G"

    #safeify commands
    #    alias rm='echo rm override deprecated 2>&1 ; command rm -i'
    #    alias cp='echo cp override deprecated 2>&1 ; command cp -i'
    #    alias mv='echo mv override deprecated 2>&1 ; command mv -i'
    alias rm='echo rm override obsoleted. Use \"rmi\" ; echo -ne \\a ; sleep 1 ; echo -ne \\a ; false'
    alias cp='echo cp override obsoleted. Use \"cpi\" ; echo -ne \\a ; sleep 1 ; echo -ne \\a ; false'
    alias mv='echo mv override obsoleted. Use \"mvi\" ; echo -ne \\a ; sleep 1 ; echo -ne \\a ; false'
    #    alias rmi='command rm -i'
    #    alias cpi='command cp -i'
    #    alias mvi='command mv -i'

    #some functions that can't be done by aliases

    # puppet functionality
    function validate_yaml { ruby -ryaml -e "YAML.load_file '$1'" ; }
    function validate_erb { erb -P -x -T '-' $1 | ruby -c ; }

    function l-() {
        # shellcheck disable=SC2145   # incorrect interpretation of what the code is successfully doing: prepending "-" to to only the first of set of args provided
        command ls -lA $enablecolor -"$@"
    }
    function ls-() {
        # shellcheck disable=SC2145   # incorrect interpretation of what the code is successfully doing: prepending "-" to to only the first of set of args provided
        command ls $enablecolor -"$@"
    }

    function cd() {
        if [ "$#" = 0 ] ; then
            pushd $HOME
        elif [ "$1" = - ] ; then
            pushd
        else
            pushd "$@"
        fi > /dev/null
    }
    function fg() {
        if [ x"$*" != x ] ; then
            generatetitle `jobs "$@"`
        else
            generatetitle `jobs | grep '^\[[0-9]\+\]+'`
        fi
        command fg "$@"
    }

    function uid() {
        id | sed 's/^[^=]*=\([0-9]*\)(\([^ ]*\)).*/\1/' 
    }

    function log-to-file() {
        local out_file="$1"
        exec > >(tee -a "$out_file" ) 2>&1
    }

}

# some functions just always need to be defined
function id_username() {
    id | sed 's/^[^=]*=\([0-9]*\)(\([^ ]*\)).*/\2/'
}

function programexists() {
    type -p "$1" >/dev/null
}

function addkeychain() {
    # prime the PIDs - may be stale, but we'll find out soon
    if [ -e $HOME/.keychain/$HOSTNAME-sh ] ; then
        . $HOME/.keychain/$HOSTNAME-sh
    fi
    if [ -e $HOME/.keychain/$HOSTNAME-sh-gpg ] ; then
        . $HOME/.keychain/$HOSTNAME-sh-gpg
    fi

    # for gpg, always use ncurses on local tty rather than display:
    GPG_TTY=$(tty) ; export GPG_TTY         # fall back to ncurses pinentry - but only if piece of shit pinentry-gnome3 is not installed, in which case it forces it to open on DISPLAY=:0 regardless of any of your settings

    case "$#,$1" in
        1,--withgpg)
            keys="$( gpg --list-secret-keys | grep ^ssb | sed 's!/! !' | awk '{print $3}' )"
            for i in {id_dsa,id_rsa,identity} ; do
                if [ -e $HOME/.ssh/$i ] ; then
                    keys="$keys $i"
                fi
            done
            (
                unset DISPLAY # doesn't suffice to say --nogui,
                              # because there's a faut in keychain
                              # where it runs gpg_listmissing before
                              # unsetting DISPLAY, and listmissing
                              # ends up asking for the same
                              # authentication

                echo "Starting keychain (perhaps in another screen)..." 1>&2
                keychain --quiet --agents gpg,ssh --nogui $keys
            )
            ;;
        0,)
            keys=
            for i in {id_dsa,id_rsa,identity} ; do
                if [ -e $HOME/.ssh/$i ] ; then
                    keys="$keys $i"
                fi
            done
            # try to be silent at first, but give up quickly and
            # mention what we're waiting on just in case the ssh
            # askpass prompt is coming up on a screen you can't see
            if ! ssh-add -l $keys | grep -v 'no identities' | grep -q . ; then
                echo "Starting keychain (perhaps in another screen)..." 1>&2
                keychain --quiet $keys
            fi
            ;;
        *)
            echo "Usage: addkeychain [--withgpg]" 1>&2
            return 1
            ;;
    esac

    # source them again in case the original had stale entries and
    # have now changed
    if [ -e $HOME/.keychain/$HOSTNAME-sh ] ; then
        . $HOME/.keychain/$HOSTNAME-sh
    fi
    if [ -e $HOME/.keychain/$HOSTNAME-sh-gpg ] ; then
        . $HOME/.keychain/$HOSTNAME-sh-gpg
    fi
}
