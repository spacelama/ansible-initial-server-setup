[ -z "$NONINTERACT" -a -z "$PBS_ENVIRONMENT" ] || return 0

ulimit -c unlimited
ulimit -S -c 0   #soft core limit. Can be increased by user

export HISTSIZE=32768
export HISTFILESIZE=32768
#  unset HISTFILE               # don't store history between sessions
export history_control= # ignoredups    # don't store equal lines in history
export HISTCONTROL= # ignoredups    # don't store equal lines in history
set +o histexpand      #I never use ! history expansion, and I do use ! literally, and acting in exactly the same way whether interactive or not.
export HISTTIMEFORMAT='%F %T '
export HISTIGNORE='*reboot:*poweroff'
export command_oriented_history=1   #try to put all commands onto single line

#  setprompt
#  cd `pwd`    #when /home/tconnors points to /home/tconnors-scuzzie, then we want to cd to set the path
RPWD=$(realpath "$PWD")
if [ "$RPWD" = $(realpath $HOME) ] ; then
    cd
fi
#    echo I hate `date +"%A"`s.
#    GLOBIGNORE='.*'   # man bash shows that . and .. are ignored with this set, but dotfiles have to be explicitly included since dotglob is then turned off (FIXME: but then .* can't be globbed explicitly - want something that .* excudes . and .., but "*" doesn't include .*
set -o noclobber
set -b #notify as soon as a program gets a signal
shopt -s checkwinsize  #when a program exits, and control is returned to the shell, we want it to check the window size again
shopt -s checkhash     #checks hash for existance so that still works if files moved
shopt -s histverify    #history subs are loaded into readline first
shopt -s histreedit    #if history fails, can go back
shopt -s no_empty_cmd_completion #good for pasting in code from scripts?
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

