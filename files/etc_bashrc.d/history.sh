function exitcleanly() {
    #    exiting=true
    # cancel all traps we set up ourselves to manage prompt and history:
    trap - 0 HUP DEBUG
    #    writetohistory --full
    writetohistory "@$HOSTNAME CLOSE"
    exit $code
}

function readfromhistory() {
    history -r
}

function writetohistory() {
    #    bt
    history=$( history -a /dev/stdout ) # doesn't clear the history because in subshell
    # echo "********history=$history"
    if [ -z "$1" -a -z "$history" ] ; then
        return
    fi
    #    set -xv
    ( (
        #    echo -n "writing history..."
        mkdir -p /tmp/$USER
        lockfile -1 -r 30 -l 60 /tmp/$USER/.bash_history.lock
        PWDENC="${PWD//!/\\!}"
        history=$(
            if [ -n "$1" ] ; then
                echo "#"$(date +%s)
                echo "#######################"
            else
                echo "$history"
            fi | sed "/^#[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$/s!\$! $PTS ${OPVIEW_VIEW:+$OPVIEW_VIEW/$OPVIEW_ITEM }$PWDENC$1!"
               )
        if [ -n "$history" ] ; then
            echo "$history" >> $HOME/.bash_fullhistory
            if [ -z "$1" ] ; then
                echo "$history" >> $HOME/.bash_history
            fi
        fi
        #    if [ "$1" == --full ] ; then
        #        command cp $HOME/.bash_fullhistory $HOME/.bash_history
        #    fi
        command rm -f /tmp/$USER/.bash_history.lock
        #    echo done
    ) & )
    #    sleep 0.3 ; echo ; echo ; echo about to clear new history, sleeping a bit to clear race possible condition, about to dedup ; echo ; sleep 0.3
    if HISTTIMEFORMAT= history 2 | sed 's/^[ 0-9]*//' | uniq -d | grep -q . ; then
        #        echo duplicated history
        lasthist=$( history 1 | awk '{print $1}' )
#        hiliteStdErr echo "DEBUG: we found a duplicated history line - we will try to delete lasthist=$lasthist" 1>&2
#        history 10 | hiliteStdErr cat 1>&2
        echo "DEBUG: we found a duplicated history line - we will try to delete lasthist=$lasthist" 1>&2 >> /tmp/hist.$USER.dup
        history 10 | hiliteStdErr cat 1>&2 >> /tmp/hist.$USER.dup
        history -d $lasthist
    fi
    history -a /dev/null   # clear the history since the append is done in a subshell
    #    set +xv
    #    sleep 1 ; echo ; echo ; echo ; echo sleeping.5 ; sleep 0.5
}

function setup_bash_history_settings() {
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
    if [ -e $HOME/.bash_fullhistory -a "$OS" = Linux ] ; then
        #        echo reading history
        HISTSIZE2=$((HISTSIZE*2))
        tail -n $HISTSIZE2 $HOME/.bash_fullhistory > /tmp/hist.$USER.$$
        history -r /tmp/hist.$USER.$$
        command rm -f /tmp/hist.$USER.$$
    else
        touch $HOME/.bash_fullhistory
        chmod 600 $HOME/.bash_fullhistory
    fi
}
