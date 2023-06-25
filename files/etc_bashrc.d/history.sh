function exitcleanly() {
    #    exiting=true
    # cancel all traps we set up ourselves to manage prompt and history:
    trap - 0 HUP DEBUG
    writetohistory "@$HOSTNAME CLOSE"
    exit $code
}

function readfromhistory() {
    history -r
}

function writetohistory() {
    #FIXME: ideally detect when we're inside emacs TRAMP and do nothing
    #    bt
    local BASH_FULLHIST="${BASH_FULL_HISTFILE:-$HOME/.bash_fullhistory}"
    if [ "$BASH_FULL_HISTFILE_LAST" != "$BASH_FULLHIST" ] ; then
        # time to read from the new file we're just setting, and start writing to it
        BASH_FULL_HISTFILE_LAST="$BASH_FULLHIST"
        setup_bash_history_file
    fi
    history=$( history -a /dev/stdout ) # doesn't clear the history because in subshell
    if [ -z "$1" -a -z "$history" ] ; then
        return
    fi
    ( (
        # writing history...
        mkdir -p /tmp/$USER
        lockfile -1 -r 30 -l 60 /tmp/$USER/.bash_history.lock
        PWDENC="${PWD//!/\\!}"
        history=$(
            if [ -n "$1" ] ; then
                echo "#"$(date +%s)
                #                    echo "#######################"  # doesn't seem to be necessary in 2023 anymore - don't end up with history composed of dates anymore
            else
                echo "$history"
            fi | sed "/^#[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$/s!\$! $PTS ${OPVIEW_VIEW:+$OPVIEW_VIEW/$OPVIEW_ITEM }$PWDENC$1!"
               )
        if [ -n "$history" ] ; then
            echo "$history" >> "$BASH_FULLHIST"
        fi
        command rm -f /tmp/$USER/.bash_history.lock
    ) & )
    if HISTTIMEFORMAT= history 2 | sed 's/^[ 0-9]*//' | uniq -d | grep -q . ; then
        # duplicated history
        lasthist=$( history 1 | awk '{print $1}' )
        echo "DEBUG: we found a duplicated history line - we will try to delete lasthist=$lasthist" 1>&2 >> /tmp/hist.$USER.dup
        history 10 | hiliteStdErr cat 1>&2 >> /tmp/hist.$USER.dup
        history -d $lasthist
    fi
    history -a /dev/null   # clear the history since the append is done in a subshell
}

function setup_bash_history_file() {
    local BASH_FULLHIST="${BASH_FULL_HISTFILE:-$HOME/.bash_fullhistory}"

    if [ -e "$BASH_FULLHIST" -a "$OS" = Linux ] ; then
        #        echo reading history
        HISTSIZE2=$((HISTSIZE*2)) # only an approximation since we
                                  # have multiline history
        tail -n $HISTSIZE2 "$BASH_FULLHIST" > /tmp/hist.$USER.$$
        history -r /tmp/hist.$USER.$$
        command rm -f /tmp/hist.$USER.$$

        #        if HISTTIMEFORMAT= history 1 | grep '^[0-9]*  *#######################$' ; then
        #            history -d -1
        #        fi
    else
        touch "$BASH_FULLHIST"
        chmod 600 "$BASH_FULLHIST"
    fi
}

function setup_bash_history_settings() {
#    echo setup_bash_history_settings 1>&2
    export HISTSIZE=32768
    export HISTFILESIZE=32768
    #  unset HISTFILE               # don't store history between sessions
    export history_control= # ignoredups    # don't store equal lines in history
    export HISTCONTROL= # ignoredups    # don't store equal lines in history
    set +o histexpand      #I never use ! history expansion, and I do use ! literally, and acting in exactly the same way whether interactive or not.
    export HISTTIMEFORMAT='%F %T '
    export HISTIGNORE='*reboot:*poweroff'
    export command_oriented_history=1   #try to put all commands onto single line

    shopt -s histverify    #history subs are loaded into readline first
    shopt -s histreedit    #if history fails, can go back
    #    shopt -s cmdhist       #save all lines as one in history
    shopt -s lithist       #20170616 it seems that saving hist with timestamps allows us then to to read history back into a new session (even on bash3?) and still get all the literal newlines correctly.  So go back to lithist since it's so much better not having your history munged
    # see /etc/inputrc for show-all-if-ambiguous

    setup_bash_history_file
}
